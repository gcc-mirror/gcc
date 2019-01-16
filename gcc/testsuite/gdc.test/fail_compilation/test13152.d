/*
TEST_OUTPUT:
---
fail_compilation/test13152.d(11): Error: undefined identifier `x`
---
*/
import imports.test13152a;

void main()
{
    auto y = x;
}
