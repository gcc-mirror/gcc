/*
TEST_OUTPUT:
---
fail_compilation/fail5953s2.d(12): Error: expression expected, not `,`
fail_compilation/fail5953s2.d(12): Error: expression expected, not `,`
fail_compilation/fail5953s2.d(12): Error: expression expected, not `,`
---
*/
void main()
{
    struct S{}
    S s3 = {,,,}; // invalid, but compiles
}
