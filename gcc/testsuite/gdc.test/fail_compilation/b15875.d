/* TEST_OUTPUT:
---
fail_compilation/b15875.d(9): Error: circular reference to variable `a`
fail_compilation/b15875.d(10): Error: circular reference to `b15875.f`
---
*/
// https://issues.dlang.org/show_bug.cgi?id=15875
// https://issues.dlang.org/show_bug.cgi?id=17290
d o(int[a]a)(){}
f.T f(){}
