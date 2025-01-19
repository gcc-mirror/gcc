/*
TEST_OUTPUT:
---
fail_compilation/binexperr.d(12): Error: expression expected, not `)`
fail_compilation/binexperr.d(12): Error: missing closing `)` after `if (A1 * (__error)`
---
*/

void main()
{
    struct A1 {}
    if (A1*) {}
    return;
}
