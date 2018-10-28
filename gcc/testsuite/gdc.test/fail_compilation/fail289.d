/*
TEST_OUTPUT:
---
fail_compilation/fail289.d(12): Error: cannot cast from function pointer to delegate
---
*/

alias void delegate() Dg;
void fun() {}
void gun()
{
    Dg d = cast(void delegate())&fun;
}
