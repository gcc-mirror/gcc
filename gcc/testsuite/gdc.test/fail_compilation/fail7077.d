/*
TEST_OUTPUT:
---
fail_compilation/fail7077.d(11): Error: undefined identifier `x`
---
*/

void main()
{
    if(0) mixin("auto x = 2;");
    assert(x == 2);
}
