/*
REQUIRED_ARGS: -preview=fixImmutableConv
TEST_OUTPUT:
---
fail_compilation/void_cat.d(15): Error: cannot copy `const(void)[]` to `void[]`
fail_compilation/void_cat.d(15):        Source data has incompatible type qualifier(s)
fail_compilation/void_cat.d(15):        Use `cast(void[])` to force copy
fail_compilation/void_cat.d(19): Error: cannot append type `const(void)[]` to type `void[]`
---
*/

void g(int*[] a, const(int*)[] b) @system
{
    void[] va = a;
    va[] = va.init ~ b; // a now contains b's data
    *a[0] = 0; // modify const data

    const(void)[] vb = b;
    va ~= vb; // also leaks const pointers into void[]
    // va could be copied into `a` via another void[]
}
