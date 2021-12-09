/*
TEST_OUTPUT:
---
fail_compilation/fail20538.d(12): Error: assignment must be preceded by an identifier
fail_compilation/fail20538.d(12): Error: found `1` when expecting `,`
---
*/

enum smth
{
    a,
    = 1,
    @disable b
}
