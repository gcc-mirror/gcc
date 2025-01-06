/**
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test24694.d(25): Error: reference to local variable `x` assigned to non-scope `b.c.p`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=24694

class C
{
    int* p;
}

struct S
{
    C c;
}

int* escape() @safe
{
    int x = 0;
    S b = S(new C());
    b.c.p = &x;
    return b.c.p;
}
