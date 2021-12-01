// PERMUTE_ARGS: -w -dw -de -d

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=6652

/*
TEST_OUTPUT:
---
fail_compilation/fail6652.d(20): Error: cannot modify `const` expression `i`
fail_compilation/fail6652.d(25): Error: cannot modify `const` expression `i`
fail_compilation/fail6652.d(30): Error: cannot modify `const` expression `i`
fail_compilation/fail6652.d(35): Error: cannot modify `const` expression `i`
---
*/

void main()
{
    foreach (const i; 0..2)
    {
        ++i;
    }

    foreach (ref const i; 0..2)
    {
        ++i;
    }

    foreach (const i, e; [1,2,3,4,5])
    {
        ++i;
    }

    foreach (ref const i, e; [1,2,3,4,5])
    {
        ++i;
    }
}
