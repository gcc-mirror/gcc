// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
fail_compilation/ice15317.d(11): Error: undefined identifier `fun`
---
*/

void main()
{
    alias f = fun;
    auto x1 = &f;
}
