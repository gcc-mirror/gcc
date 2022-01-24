/*
TEST_OUTPUT:
---
fail_compilation/fail12390.d(14): Error: `fun().i == 4` has no effect
---
*/

struct S { int i; }

S fun() { return S(42); }

void main()
{
    fun().i == 4;
}
