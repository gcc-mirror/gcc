/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test22541.d(104): Error: escaping a reference to parameter `i` by returning `i` is not allowed in a `@safe` function
fail_compilation/test22541.d(102):        perhaps annotate the parameter with `return`
---
 */

/* https://issues.dlang.org/show_bug.cgi?id=22541
 */

#line 100

@safe
ref int newe(ref return scope int i) // ref, error
{
    return i;
}
