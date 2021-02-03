/*
TEST_OUTPUT:
---
fail_compilation/fail7424i.d(10): Error: cannot resolve type for this.g()()
---
*/
struct S7424g
{
    @property int g()() immutable { return 0; }
    void test() inout { int f = g; }
}

