/*
TEST_OUTPUT:
---
fail_compilation/const_ctor.d(23): Error: `const` copy constructor `const_ctor.S1.this` cannot construct a mutable object
fail_compilation/const_ctor.d(25): Error: `const` constructor `const_ctor.S2.this` cannot construct a mutable object
---
*/

struct S1
{
    this(ref const S1 s) const {}
    int* i;
}
struct S2
{
    this(int) const {}
    int* i;
}

void main()
{
    const(S1) s1;
    S1 m1 = s1;

    S2 s2 = S2(5);
}
