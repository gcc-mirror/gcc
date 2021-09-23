// https://issues.dlang.org/show_bug.cgi?id=21927
/*
TEST_OUTPUT:
---
fail_compilation/test21927.d(17): Error: invalid `foreach` aggregate `this.T2(Args2...)`
fail_compilation/test21927.d(18): Error: invalid `foreach` aggregate `this.T2!()`
---
*/

struct S
{
    template T2(Args2...) {}

    void fun()
    {
        // original test case
        static foreach (p; this.T2) {} // ICE
        static foreach (p; this.T2!()) {} // ICE
    }
}
