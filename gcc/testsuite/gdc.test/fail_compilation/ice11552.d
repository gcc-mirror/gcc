/*
REQUIRED_ARGS: -o-
PERMUTE_ARGS:
TEST_OUTPUT:
---
fail_compilation/ice11552.d(14): Error: label `label` is undefined
fail_compilation/ice11552.d(17):        called from here: test11552()
fail_compilation/ice11552.d(17):        while evaluating: `static assert(test11552())`
---
*/

int test11552()
{
    goto label;
    return 1;
}
static assert(test11552());
