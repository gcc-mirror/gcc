/*
TEST_OUTPUT:
---
fail_compilation/fail19931.d(10): Error: `struct S` may not define both a rvalue constructor and a copy constructor
fail_compilation/fail19931.d(12):        rvalue constructor defined here
fail_compilation/fail19931.d(13):        copy constructor defined here
---
*/

struct S
{
    this(S s) {}
    this(ref S s) {}
    this(this) {}
}
