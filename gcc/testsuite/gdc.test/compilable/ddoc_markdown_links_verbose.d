// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o- -transition=vmarkdown
// TEST_OUTPUT_FILE: extra-files/ddoc_markdown_links_verbose.html
// OUTPUT_FILES: ${RESULTS_DIR}/compilable/ddoc_markdown_links_verbose.html

/++
Links:

A link to [Object].
An inline link to [the D homepage](https://dlang.org).
A simple link to [dub].
A slightly less simple link to [dub][].
An image: ![D-Man](https://dlang.org/images/d3.png)

[dub]: https://code.dlang.org
+/
module test.compilable.ddoc_markdown_links_verbose;
