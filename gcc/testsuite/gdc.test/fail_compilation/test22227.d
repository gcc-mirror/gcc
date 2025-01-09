/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test22227.d(12): Error: returning scope variable `foo` is not allowed in a `@safe` function
fail_compilation/test22227.d(14): Error: returning scope variable `foo` is not allowed in a `@safe` function
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
