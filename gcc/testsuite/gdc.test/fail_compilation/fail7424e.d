/*
TEST_OUTPUT:
---
fail_compilation/fail7424e.d(10): Error: cannot resolve type for this.g()()
---
*/
struct S7424e
{
    @property int g()() immutable { return 0; }
    void test() { int f = g; }
}

