// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// TEST_OUTPUT_FILE: extra-files/ddoc_markdown_escapes.html
// OUTPUT_FILES: ${RESULTS_DIR}/compilable/ddoc_markdown_escapes.html

/++
Backslash Escapes:

\!\"\#\$\%\&\'\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\\\]\^\_\`\{\|\}

But not in code:

---
\{\}
---

`\{\}`

Nor in HTML:

<tag attr="\{\}"></tag>

Nor before things that aren't punctuation:

C:\dlang\dmd
+/
module ddoc_markdown_escapes;
