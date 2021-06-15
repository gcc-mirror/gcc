/*
TEST_OUTPUT:
---
fail_compilation/fail7424c.d(10): Error: expression `this.g()()` is `void` and has no value
---
*/
struct S7424c
{
    @property int g()() { return 0; }
    void test() immutable { int f = g; }
}

