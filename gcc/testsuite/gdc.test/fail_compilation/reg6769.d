/*
TEST_OUTPUT:
---
fail_compilation/reg6769.d(14): Error: reinterpreting cast from `int[]` to `int[7]*` is not supported in CTFE
fail_compilation/reg6769.d(27):        called from here: `reg6769a([0, 1, 2, 3, 4, 5, 6])`
fail_compilation/reg6769.d(27):        while evaluating: `static assert(reg6769a([0, 1, 2, 3, 4, 5, 6]) == 1)`
fail_compilation/reg6769.d(20): Error: reinterpreting cast from `int[7]` to `int[]*` is not supported in CTFE
fail_compilation/reg6769.d(28):        called from here: `reg6769b([0, 1, 2, 3, 4, 5, 6])`
fail_compilation/reg6769.d(28):        while evaluating: `static assert(reg6769b([0, 1, 2, 3, 4, 5, 6]) == 1)`
---
*/
int reg6769a(int[] a)
{
    int[7]* b = cast(int[7]*)&a;
    return (*b)[1];
}

int reg6769b(int[7] a)
{
    int[]* b = cast(int[]*)&a;
    return (*b)[1];
}

void main()
{
    // Both should never succeed, run-time would raise a SEGV.
    static assert(reg6769a([0,1,2,3,4,5,6]) == 1);
    static assert(reg6769b([0,1,2,3,4,5,6]) == 1);
}
