// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// TEST_OUTPUT_FILE: extra-files/ddoc_markdown_code.html
// OUTPUT_FILES: ${RESULTS_DIR}/compilable/ddoc_markdown_code.html

/++
# Code Blocks

---
A regular ol' code block (note the uneven delimiter lengths)
----

```
A backtick-fenced code block
```

~~~
A tilde-fenced code block
~~~

--- d
A hyphen-fenced D code block
---

--- d delish
A backtick-fenced D code block
---

~~~ d
A tilde-fenced D code block
~~~

--- ruby
A hyphen-fenced ruby code block
---

--- ruby delish
A backtick-fenced ruby code block
---

~~~ ruby
A tilde-fenced ruby code block
~~~

+/
module test.compilable.ddoc_markdown_code;
