/*
TEST_OUTPUT:
---
fail_compilation/diag12678.d(21): Error: const field `cf1` initialized multiple times
fail_compilation/diag12678.d(20):        Previous initialization is here.
fail_compilation/diag12678.d(24): Error: immutable field `if1` initialized multiple times
fail_compilation/diag12678.d(23):        Previous initialization is here.
fail_compilation/diag12678.d(27): Error: const field `cf2` initialization is not allowed in loops or after labels
---
*/

struct S
{
    const int cf1;
    const int cf2;
    immutable int if1;

    this(int x)
    {
        cf1 = x;
        cf1 = x;

        if1 = x;
        if1 = x;

        foreach (i; 0 .. 5)
            cf2 = x;
    }
}
