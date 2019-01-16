/*
TEST_OUTPUT:
---
fail_compilation/ice12581.d(21): Error: undefined identifier `undef`
---
*/

struct S
{
    int[3] a;
    alias a this;
}
struct T
{
    S s;
    alias s this;
}
void main()
{
    T x;
    x[] = (undef = 1);
}
