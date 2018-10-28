/*
TEST_OUTPUT:
---
fail_compilation/ice9291.d(10): Error: undefined identifier `F`
---
*/

void main() nothrow
{
    throw new F();
}
