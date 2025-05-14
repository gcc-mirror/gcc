// https://issues.dlang.org/show_bug.cgi?id=22151
/*
TEST_OUTPUT:
---
fail_compilation/fail22151.d(17): Error: function `test` is not an lvalue and cannot be modified
fail_compilation/fail22151.d(18): Error: function `test2` is not an lvalue and cannot be modified
fail_compilation/fail22151.d(21): Error: function pointed to by `fp` is not an lvalue and cannot be modified
fail_compilation/fail22151.d(24): Error: function pointed to by `ff` is not an lvalue and cannot be modified
fail_compilation/fail22151.d(27): Error: operator `==` is not defined for function types
fail_compilation/fail22151.d(28): Error: operator `is` is not defined for function types
fail_compilation/fail22151.d(29): Error: comparison is not defined for function types
---
*/

void test()
{
    *&test = *&test;
    *&test2 = *&test;

    void function() fp;
    *fp = *fp;

    auto ff = &test2;
    *ff = *&test2;

    // https://github.com/dlang/dmd/issues/18281
    const c = *fp == *fp;
    const d = *fp is *fp;
    const e = *fp < *fp;
}

void test2();
