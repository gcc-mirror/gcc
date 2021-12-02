/*
TEST_OUTPUT:
---
fail_compilation/fail325.d(12): Error: template `fun(T = int)(int w, int z)` has no type
---
*/

void fun(T = int)(int w, int z) {}

void main()
{
    auto x = cast(void function(int, int))fun;
}
