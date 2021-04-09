/*
TEST_OUTPUT:
---
fail_compilation/fail5953s1.d(10): Error: expression expected, not `,`
---
*/
void main()
{
    struct S{}
    S s2 = {,};   // invalid, but compiles
}
