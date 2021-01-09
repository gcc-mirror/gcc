/*
TEST_OUTPUT:
---
fail_compilation/fail7424c.d(10): Error: cannot resolve type for this.g()()
---
*/
struct S7424c
{
    @property int g()() { return 0; }
    void test() immutable { int f = g; }
}

