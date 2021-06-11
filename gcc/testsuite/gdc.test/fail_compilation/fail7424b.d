/*
TEST_OUTPUT:
---
fail_compilation/fail7424b.d(10): Error: expression `this.g()()` is `void` and has no value
---
*/
struct S7424b
{
    @property int g()() { return 0; }
    void test() const { int f = g; }
}
