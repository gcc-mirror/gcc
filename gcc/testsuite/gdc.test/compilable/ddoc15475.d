/* PERMUTE_ARGS:
REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh
*/

/**
My module
----
   // Computes the interval [x,y)
   auto interval = computeInterval(x, y);
----

Backslash-escape parentheses with `\(` and `\)`.

---
(
---

---
)
---

---
    Here are some nested `backticks`
    // Another interval [x,y)
---

---
    This won't end the code block: --- )
    // Yet another interval [x,y)
---
*/
module ddoc15475;
