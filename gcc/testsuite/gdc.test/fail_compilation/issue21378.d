/*
TEST_OUTPUT:
---
fail_compilation/issue21378.d(13): Error: function `issue21378.fn` circular dependency. Functions cannot be interpreted while being compiled
fail_compilation/issue21378.d(12):        called from here: `fn()`
fail_compilation/issue21378.d(12): Error: pragma(`inline`, `true` or `false`) expected, not `fn()`
---
*/

// Cannot call the same function linked to the pragma
// Really hard to fix this limitation in the implementation
pragma(inline, fn())
int fn()
{
    return 1;
}
