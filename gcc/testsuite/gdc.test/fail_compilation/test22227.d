/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test22227.d(12): Error: scope variable `foo` may not be returned
fail_compilation/test22227.d(14): Error: scope variable `foo` may not be returned
---
*/

int[] foo() @safe
{
    if (scope foo = [1])
        return foo;
    while (scope foo = [1])
        return foo;
    return [];
}
