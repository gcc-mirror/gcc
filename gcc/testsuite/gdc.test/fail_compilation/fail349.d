/*
TEST_OUTPUT:
---
fail_compilation/fail349.d(15): Error: function `fail349.bug6109throwing` is not `nothrow`
fail_compilation/fail349.d(13): Error: `nothrow` function `fail349.bug6109noThrow` may throw
---
*/

int bug6109throwing()
{
    throw new Exception("throws");
}
int bug6109noThrow() nothrow
{
    auto g = [4][0 .. bug6109throwing()];
    return 0;
}
