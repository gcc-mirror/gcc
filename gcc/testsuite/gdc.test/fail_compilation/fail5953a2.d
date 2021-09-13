/*
TEST_OUTPUT:
---
fail_compilation/fail5953a2.d(11): Error: expression expected, not `,`
fail_compilation/fail5953a2.d(11): Error: expression expected, not `,`
fail_compilation/fail5953a2.d(11): Error: expression expected, not `,`
---
*/
void main()
{
    auto a3 = [,,,];    // invalid, but compiles
}
