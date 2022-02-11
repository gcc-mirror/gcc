// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// TEST_OUTPUT_FILE: extra-files/ddoc_markdown_breaks.html
// OUTPUT_FILES: ${RESULTS_DIR}/compilable/ddoc_markdown_breaks.html

/++
# Thematic Breaks

Some text before
***
Some text in between
____________________
Some text after

---
This is a code block
---

But this is a thematic break:

- - -

## Not Thematic Breaks

- -
__
**

+/
module ddoc_markdown_lists;
