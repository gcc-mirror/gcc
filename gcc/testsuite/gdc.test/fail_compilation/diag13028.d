/*
TEST_OUTPUT:
---
fail_compilation/diag13028.d(15): Error: variable `dg` cannot be read at compile time
fail_compilation/diag13028.d(22): Error: variable `a` cannot be read at compile time
fail_compilation/diag13028.d(28): Error: CTFE failed because of previous errors in `foo`
fail_compilation/diag13028.d(28):        while evaluating: `static assert(foo(() pure nothrow @nogc @safe => 1) == 1)`
fail_compilation/diag13028.d(29): Error: CTFE failed because of previous errors in `bar`
fail_compilation/diag13028.d(29):        while evaluating: `static assert(bar(delegate int() pure nothrow @nogc @safe => 1) == 1)`
---
*/

int foo(int delegate() dg)
{
    enum b = dg();
    return b;
}


int bar(lazy int a)
{
    enum b = a;
    return a;
}

void main()
{
    static assert(foo(() => 1) == 1);
    static assert(bar(1) == 1);
}
