/*
TEST_OUTPUT:
---
fail_compilation/fail6561.d(9): Error: undefined identifier `x`
---
*/
struct S
{
    alias x this;   // should cause undefined identifier error
}

void main()
{
}
