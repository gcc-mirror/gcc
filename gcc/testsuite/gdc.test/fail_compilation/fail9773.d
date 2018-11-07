/*
TEST_OUTPUT:
---
fail_compilation/fail9773.d(7): Error: "" is not an lvalue
---
*/
void f(ref string a = "")
{
    a = "crash and burn";
}
