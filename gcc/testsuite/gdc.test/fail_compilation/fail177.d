/*
TEST_OUTPUT:
---
fail_compilation/fail177.d(22): Error: cannot modify immutable expression j
fail_compilation/fail177.d(24): Error: cannot modify const expression i
fail_compilation/fail177.d(26): Error: cannot modify const expression s1.x
fail_compilation/fail177.d(27): Error: cannot modify const expression *s1.p
fail_compilation/fail177.d(29): Error: cannot modify const expression s2.x
fail_compilation/fail177.d(30): Error: cannot modify const expression *s2.p
---
*/

struct S
{
    int x;
    int* p;
}

void test(const(S) s1, const S s2, const(int) i)
{
    immutable int j = 3;
    j = 4;

    i = 4;

    s1.x = 3;
    *s1.p = 4;

    s2.x = 3;
    *s2.p = 4;
}
