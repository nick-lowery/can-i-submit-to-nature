Can I publish this in Nature? An NLP approach
================
2018-11-01

In attempts to teach myself something about natural language processesing, scientific articles seemed like a good corpus that I'm familiar with and would be reasonably amenable to various NLP pipelines.  So, given my disillusionment with the notion of "impact" in scientific publishing, a guide to whether one's manuscript was suitable for publication in Nature or some lesser rickety backwater publication (like Nature Communications) seemed to strike an appropriate balance of tractability and snark.  

(Also, a disclaimer: in the interest of silliness, while the following analyses are (more or less) legitimate, my discussion of the results will range from sarcastic to willfil misinterpretation; for instance, the tf-idf analyses probably represent one or just a few articles that use a given term a *ton*, but it's more fun to treat them as general principles.  However, if you do incorporate this advice and get accepted in Nature don't forget to give a mention in the acknowledgements.)

## What's done
* crawling and scraping the Nature website for article text ([crawling_nature.Rmd](/crawling_nature.Rmd))
* exploratory word frequency / tf-idf analysis (also [crawling_nature.Rmd](/crawling_nature.Rmd))

## On the docket
* building a classifier, for when it's time to submit
* attempt to train an RNN to write Nature articles for me
