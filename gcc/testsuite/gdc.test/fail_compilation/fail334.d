/*
TEST_OUTPUT:
---
fail_compilation/fail334.d(10): Error: properties can only have zero, one, or two parameter
---
*/

struct S
{
    @property int foo(int a, int b, int c) { return 1; }
}
