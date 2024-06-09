/*
TEST_OUTPUT:
---
fail_compilation/fail136.d(10): Error: `x"EFBBBF"` has no effect
---
*/

void main()
{
    x"EF BB BF";
}
