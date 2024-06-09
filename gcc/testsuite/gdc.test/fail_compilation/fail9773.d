/*
TEST_OUTPUT:
---
fail_compilation/fail9773.d(7): Error: cannot create default argument for `ref` / `out` parameter from expression `""` because it is not an lvalue
---
*/
void f(ref string a = "")
{
    a = "crash and burn";
}
