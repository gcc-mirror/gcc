/*
TEST_OUTPUT:
---
fail_compilation/diag12678.d(19): Error: const field 'cf1' initialized multiple times
fail_compilation/diag12678.d(22): Error: immutable field 'if1' initialized multiple times
fail_compilation/diag12678.d(25): Error: const field 'cf2' initialization is not allowed in loops or after labels
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
