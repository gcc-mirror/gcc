/*
TEST_OUTPUT:
---
fail_compilation/fail12901.d(11): Error: constructor fail12901.S.this in and out contracts require function body
---
*/

struct S
{
    int a;
    this(int n)
    in { a = n; }
    // no body
}
