/*
TEST_OUTPUT:
---
fail_compilation/diag10099.d(15): Error: variable diag10099.main.s default construction is disabled for type S
---
*/

struct S
{
    @disable this();
}

void main()
{
    S s;
}
