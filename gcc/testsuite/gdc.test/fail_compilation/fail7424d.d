/*
TEST_OUTPUT:
---
fail_compilation/fail7424d.d(10): Error: expression `this.g()()` is `void` and has no value
---
*/
struct S7424d
{
    @property int g()() immutable { return 0; }
    void test() const { int f = g; }
}

