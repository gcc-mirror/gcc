// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
fail_compilation/diag15001.d(11): Error: undefined identifier `X`
---
*/

void main()
{
    if (X x = 1)
    {
    }
}
