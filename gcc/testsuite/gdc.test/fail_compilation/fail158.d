/*
TEST_OUTPUT:
---
fail_compilation/fail158.d(17): Error: more initializers than fields (2) of `S`
---
*/

struct S
{
    int i;
    int j = 3;
}


void main()
{
    S s = S( 1, 5, 6 );
}
