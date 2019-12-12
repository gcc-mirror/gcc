/*
TEST_OUTPUT:
---
fail_compilation/fail13701.d(16): Error: cannot modify immutable expression this.aa[10]
fail_compilation/fail13701.d(23): Error: cannot modify immutable expression aa[10]
fail_compilation/fail13701.d(24): Error: cannot modify immutable expression aa[10]
---
*/

struct S
{
    immutable(int)[int] aa;
    this(int n)
    {
        aa[10] = 20;    // initializing
        aa[10] = 30;    // assignment
    }
}

void main()
{
    immutable(int)[int] aa;
    aa[10] = 20;
    aa[10]++;
}
