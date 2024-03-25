/*
TEST_OUTPUT:
---
fail_compilation/fail7424i.d(10): Error: template `this.g()() immutable` has no value
---
*/
struct S7424g
{
    @property int g()() immutable { return 0; }
    void test() inout { int f = g; }
}
