// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// TEST_OUTPUT_FILE: extra-files/ddoc_markdown_emphasis.html
// OUTPUT_FILES: ${RESULTS_DIR}/compilable/ddoc_markdown_emphasis.html

/++
Markdown Emphasis:

*emphasized text*

*emphasized text
on more than one line*

**strongly emphasized text**

*in*seperable

***tricky** emphasis*

*more **tricky emphasis***

*tricky**unspaced***

*more**tricky**unspaced*

*highly **nested *emphasis* in this** line*

$(B *inside a macro*)

*$(B outside a macro)*

**The following aren't Markdown emphasis:**

a * not emphasis*

a*"not either"*

*nor this *

*jumping $(B into a macro*)

$(B *jumping) out of a macro*

+/
module ddoc_markdown_emphasis;
