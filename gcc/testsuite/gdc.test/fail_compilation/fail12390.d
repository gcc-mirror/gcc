/*
TEST_OUTPUT:
---
fail_compilation/fail12390.d(14): Error: `==` has no effect in expression `fun().i == 4`
---
*/

struct S { int i; }

S fun() { return S(42); }

void main()
{
    fun().i == 4;
}
