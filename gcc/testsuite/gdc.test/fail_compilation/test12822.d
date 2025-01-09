/*
TEST_OUTPUT:
---
fail_compilation/test12822.d(13): Error: modifying delegate pointer `dg.ptr` is not allowed in a `@safe` function
fail_compilation/test12822.d(14): Error: using `dg.funcptr` is not allowed in a `@safe` function
---
*/

// https://issues.dlang.org/show_bug.cgi?id=12822
void test2(int delegate() dg) @safe
{
    static int i;
    dg.ptr = &i;
    dg.funcptr = &func;
}

int func();
