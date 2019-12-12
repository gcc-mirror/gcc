/*
TEST_OUTPUT:
---
fail_compilation/fail331.d(10): Error: cannot use typeof(return) inside function foo with inferred return type
---
*/

auto foo()
{
    typeof(return) result;
    return result;
}
