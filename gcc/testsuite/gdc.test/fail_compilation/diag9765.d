/*
TEST_OUTPUT:
---
fail_compilation/diag9765.d(9): Error: cannot implicitly convert expression `'x'` of type `char` to `char[]`
---
*/

struct S9765 { char[] x; }
const S9765 s9765 = S9765('x');
const char s9765b = s9765.x;
