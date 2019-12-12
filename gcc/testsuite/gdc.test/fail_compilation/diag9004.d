/*
TEST_OUTPUT:
---
fail_compilation/diag9004.d(21): Error: template diag9004.bar cannot deduce function from argument types !()(Foo!int, int), candidates are:
fail_compilation/diag9004.d(14):        diag9004.bar(FooT)(FooT foo, FooT.T x)
---
*/

struct Foo(_T)
{
    alias _T T;
}

void bar(FooT)(FooT foo, FooT.T x)
{
}

void main()
{
    Foo!int foo;
    bar(foo, 1);
}
