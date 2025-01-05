/*
TEST_OUTPUT:
---
fail_compilation/deprecate1553.d(16): Error: cannot use `foreach_reverse` with a delegate
---
*/

struct S
{
    int dg(int delegate(ref int a)) { return 0; }
}

void main()
{
    S s;
    foreach_reverse(a; &s.dg) {}
}
