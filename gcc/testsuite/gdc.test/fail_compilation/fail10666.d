// REQUIRED_ARGS: -c
/*
TEST_OUTPUT:
---
fail_compilation/fail10666.d(16): Error: variable `fail10666.foo10666.s1` has scoped destruction, cannot build closure
---
*/


struct S10666
{
    int val;
    ~this() {}
}

void foo10666(S10666 s1)
{
    auto f1 = (){ return () => s1.val; }(); // NG

    S10666 s2;
    auto f2 = (){ return () => s2.val; }(); // (should be NG)
}
