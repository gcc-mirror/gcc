/*
REQUIRED_ARGS: -o-
TEST_OUTPUT:
---
fail_compilation/ice11552.d(13): Error: function `ice11552.test11552` label `label` is undefined
fail_compilation/ice11552.d(16):        called from here: `test11552()`
fail_compilation/ice11552.d(16):        while evaluating: `static assert(test11552())`
---
*/

int test11552()
{
    goto label;
    return 1;
}
static assert(test11552());
