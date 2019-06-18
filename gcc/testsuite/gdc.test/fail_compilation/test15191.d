/* TEST_OUTPUT:
---
fail_compilation/test15191.d(17): Error: cannot take address of `ref return` of `foo()` in `@safe` function `bar`
---
*/


// https://issues.dlang.org/show_bug.cgi?id=15191

ref int foo(return ref int s)@safe
{
    return s;
}

int* bar(return ref int s) @safe
{
    return &foo(s);
}
