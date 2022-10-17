// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// TEST_OUTPUT_FILE: extra-files/ddoc_markdown_quote.html
// OUTPUT_FILES: ${RESULTS_DIR}/compilable/ddoc_markdown_quote.html

/++
# Quote Blocks

> “It seems to me that most of the ‘new’ programming languages fall into one of
two categories: Those from academia with radical new paradigms and those from
large corporations with a focus on RAD and the web. Maybe it’s time for a new
language born out of practical experience implementing compilers.” -- Michael

> Great, just what I need.. another D in programming. -- Segfault

> To D, or not to D. -- Willeam NerdSpeare

> "What I am going to tell you about is what we teach our programming students in the third or fourth year of graduate school... It is my task to convince you not to turn away because you don't understand it. You see my programming students don't understand it... That is because I don't understand it. Nobody does."
-- Richard Deeman

Here's a bit of text between quotes.

> This is a quote
> > And this is a nested quote

> This is a quote with a > symbol in it

> This is a quote
with a continuation

> This quote
- is ended by this list

> This quote
### is ended by this heading

> This quote is ended by a thematic break:
____

> This quote
```
is ended by this code
```

> ### Some things inside a quote block:
> ___
> ---
> Some code
> ---
> - a list

+/
module test.compilable.ddoc_markdown_code;
