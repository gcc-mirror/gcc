/*
TEST_OUTPUT:
---
fail_compilation/diag9861.d(8): Error: no property `epsilon` for type `int`
fail_compilation/diag9861.d(9):        while looking for match for `Foo!int`
---
*/
struct Foo(T, real x = T.epsilon) {}
Foo!(int) q;
