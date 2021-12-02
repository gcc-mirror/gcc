/*
TEST_OUTPUT:
---
fail_compilation/test12822.d(13): Error: cannot modify delegate pointer in `@safe` code `dg.ptr`
fail_compilation/test12822.d(14): Error: `dg.funcptr` cannot be used in `@safe` code
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
