/*
TEST_OUTPUT:
---
fail_compilation/diag9004.d(21): Error: template `diag9004.bar` is not callable using argument types `!()(Foo!int, int)`
fail_compilation/diag9004.d(14):        Candidate is: `bar(FooT)(FooT foo, FooT.T x)`
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
