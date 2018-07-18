/* { dg-do compile { target size32plus } } */
/* { dg-options "-O2 -ftree-loop-distribution -ftree-loop-distribute-patterns -fdump-tree-ldist" } */

void fill(char* A, char* B, unsigned n)
{
    for (unsigned i = 0; i < n; i++)
    {
        A[i] = 0;
        B[i] = A[i] + 1;
    }
}

/* { dg-final { scan-tree-dump-times "_builtin_memset" 2 "ldist" } } */
