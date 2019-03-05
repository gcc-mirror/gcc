/*
TEST_OUTPUT:
---
fail_compilation/fail284.d(19): Error: pure function 'fail284.foo' cannot call impure function pointer 'a'
---
*/

static int nasty;

int impure_evil_function(int x)
{
    nasty++;
    return nasty;
}

pure int foo(int x)
{
    int function(int) a = &impure_evil_function;
    return a(x);
}
