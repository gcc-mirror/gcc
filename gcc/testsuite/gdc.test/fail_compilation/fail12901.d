/*
TEST_OUTPUT:
---
fail_compilation/fail12901.d(11): Error: constructor `fail12901.S.this` `in` and `out` contracts can only appear without a body when they are virtual interface functions or abstract
---
*/

struct S
{
    int a;
    this(int n)
    in { a = n; }
    // no body
}
