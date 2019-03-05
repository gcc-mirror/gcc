
// https://issues.dlang.org/show_bug.cgi?id=17419
/* TEST_OUTPUT:
---
fail_compilation/fail17419.d(10): Error: argument to `__traits(getLinkage, 64)` is not a declaration
fail_compilation/fail17419.d(11): Error: expected 1 arguments for `getLinkage` but had 2
---
*/

enum s = __traits(getLinkage, 8 * 8);
enum t = __traits(getLinkage, 8, 8);

