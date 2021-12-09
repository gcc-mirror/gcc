// https://issues.dlang.org/show_bug.cgi?id=22151
/*
TEST_OUTPUT:
---
fail_compilation/fail22151.d(14): Error: function `test` is not an lvalue and cannot be modified
fail_compilation/fail22151.d(15): Error: function `test2` is not an lvalue and cannot be modified
fail_compilation/fail22151.d(18): Error: function pointed to by `fp` is not an lvalue and cannot be modified
fail_compilation/fail22151.d(21): Error: function pointed to by `ff` is not an lvalue and cannot be modified
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
}

void test2();
