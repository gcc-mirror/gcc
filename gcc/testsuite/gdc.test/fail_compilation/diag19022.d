/*
TEST_OUTPUT:
---
fail_compilation/diag19022.d(16): Error: immutable field `b` initialized multiple times
fail_compilation/diag19022.d(15):        Previous initialization is here.
---
*/
// https://issues.dlang.org/show_bug.cgi?id=19022

struct Foo
{
    immutable int b;
    this(int a)
    {
        b = 2;
        b = 2;
    }
}
