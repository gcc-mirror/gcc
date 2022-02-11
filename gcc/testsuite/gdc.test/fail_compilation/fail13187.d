/*
TEST_OUTPUT:
---
fail_compilation/fail13187.d(12): Error: `pure` function `fail13187.test` cannot access mutable static data `my_func_ptr`
---
*/

int function(int) pure my_func_ptr;

void test() pure
{
    my_func_ptr(1);
}
