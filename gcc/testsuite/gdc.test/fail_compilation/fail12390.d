/*
TEST_OUTPUT:
---
fail_compilation/fail12390.d(15): Error: the result of the equality expression `fun().i == 4` is discarded
fail_compilation/fail12390.d(15):        note that `fun().i` may have a side effect
---
*/

struct S { int i; }

S fun() { return S(42); }

void main()
{
    fun().i == 4;
}
