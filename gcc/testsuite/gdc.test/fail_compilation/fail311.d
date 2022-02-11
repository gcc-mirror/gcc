/*
TEST_OUTPUT:
---
fail_compilation/fail311.d(16): Error: undefined identifier `undefined`
fail_compilation/fail311.d(25): Error: template instance `fail311.foo!()` error instantiating
---
*/

template Tuple(T...)
{
    alias T Tuple;
}

void foo()()
{
    undefined x;
    foreach (i; Tuple!(2))
    {
        static assert(true);
    }
}

void main()
{
    foo!()();
}
