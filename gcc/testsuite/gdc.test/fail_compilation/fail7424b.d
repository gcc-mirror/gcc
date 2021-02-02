/*
TEST_OUTPUT:
---
fail_compilation/fail7424b.d(10): Error: cannot resolve type for this.g()()
---
*/
struct S7424b
{
    @property int g()() { return 0; }
    void test() const { int f = g; }
}
