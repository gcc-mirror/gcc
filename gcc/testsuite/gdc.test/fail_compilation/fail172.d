/*
TEST_OUTPUT:
---
fail_compilation/fail172.d(25): Error: cannot modify `const` expression `c1.x`
fail_compilation/fail172.d(26): Error: cannot modify `const` expression `c2.x`
fail_compilation/fail172.d(30): Error: cannot modify `const` expression `s1.x`
fail_compilation/fail172.d(31): Error: cannot modify `const` expression `s2.x`
---
*/

class C
{
    int x;
}

struct S
{
    int x;
}

void main()
{
    const(C) c1 = new C();
    const C  c2 = new C();
    c1.x = 3;
    c2.x = 3;

    const(S) s1;
    const S  s2;
    s1.x = 3;
    s2.x = 3;
}
