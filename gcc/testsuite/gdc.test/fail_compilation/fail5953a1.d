/*
TEST_OUTPUT:
---
fail_compilation/fail5953a1.d(9): Error: expression expected, not `,`
---
*/
void main()
{
    auto a2 = [,];    // invalid, but compiles
}
