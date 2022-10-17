/*
TEST_OUTPUT:
---
fail_compilation/fail10630.d(12): Error: cannot have `out` parameter of type `S` because the default construction is disabled
---
*/

struct S
{
    @disable this();
}
void foo(out S) {}
