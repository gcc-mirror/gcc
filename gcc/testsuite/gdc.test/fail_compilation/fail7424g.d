/*
TEST_OUTPUT:
---
fail_compilation/fail7424g.d(10): Error: cannot resolve type for this.g()()
---
*/
struct S7424g
{
    @property int g()() { return 0; }
    void test() shared { int f = g; }
}

