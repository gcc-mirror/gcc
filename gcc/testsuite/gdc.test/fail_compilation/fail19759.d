// https://issues.dlang.org/show_bug.cgi?id=19759
/* TEST_OUTPUT:
---
fail_compilation/fail19759.d(8): Error: function `fail19759.fail19759` cannot have parameter of type `float[4]` because its linkage is `extern(C++)`
fail_compilation/fail19759.d(8):        perhaps use a `float*` type instead
---
*/
extern(C++) bool fail19759(float[4] col);
