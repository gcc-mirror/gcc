/*
TEST_OUTPUT:
---
fail_compilation/fail9773.d(7): Error: `""` is not an lvalue and cannot be modified
---
*/
void f(ref string a = "")
{
    a = "crash and burn";
}
