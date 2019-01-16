/*
TEST_OUTPUT:
---
fail_compilation/fail298.d(12): Error: cannot implicitly convert expression `num1 / cast(ulong)num2` of type `ulong` to `int`
---
*/

void main()
{
    ulong num1 = 100;
    int num2 = 10;
    int result = num1 / num2;
}
