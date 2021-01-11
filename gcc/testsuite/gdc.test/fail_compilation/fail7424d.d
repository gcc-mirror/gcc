/*
TEST_OUTPUT:
---
fail_compilation/fail7424d.d(10): Error: cannot resolve type for this.g()()
---
*/
struct S7424d
{
    @property int g()() immutable { return 0; }
    void test() const { int f = g; }
}

