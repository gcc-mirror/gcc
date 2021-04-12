/* TEST_OUTPUT:
---
fail_compilation/test20919.d(12): Error: `__traits(getAttributes, int a)` does not give a valid type
---
*/
// https://issues.dlang.org/show_bug.cgi?id=20919

void foo(int a) {}

static if (is(typeof(foo) params == __parameters))
{
    __traits(getAttributes, params) a;
}
