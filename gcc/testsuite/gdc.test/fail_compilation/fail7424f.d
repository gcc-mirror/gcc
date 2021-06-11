/*
TEST_OUTPUT:
---
fail_compilation/fail7424f.d(10): Error: expression `this.g()()` is `void` and has no value
---
*/
struct S7424f
{
    @property int g()() shared { return 0; }
    void test() { int f = g; }
}

