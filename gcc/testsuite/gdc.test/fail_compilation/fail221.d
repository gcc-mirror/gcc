/*
TEST_OUTPUT:
---
fail_compilation/fail221.d(11): Error: expression `cast(void)0` is `void` and has no value
---
*/

void main()
{
    void[] data;
    data ~= cast(void) 0;
}
