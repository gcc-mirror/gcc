/* { dg-do run } */
/* { dg-options "-O -fdump-tree-optimized -fwrapv" } */

#include <limits.h>
#include <stdbool.h>

bool __attribute__ ((noipa)) test1 (unsigned a, unsigned b)
{
    return (b == 0) | (a < b);
}

bool __attribute__ ((noipa)) test2 (int a, int b)
{
    return (b == INT_MIN) | (a < b);
}

bool __attribute__ ((noipa)) test3 (unsigned a, unsigned b)
{
    return (b != 0) & (a >= b);
}

bool __attribute__ ((noipa)) test4 (int a, int b)
{
    return (b != INT_MIN) & (a >= b);
}

int main()
{
    if (!test1 (1, 0) || !test1 (1, 2) || test1 (2, 1) ||
        !test2 (1, INT_MIN) || !test2 (1, 2) || test2 (2, 1) ||
        test3 (1, 0) || test3 (1, 2) || !test3 (2, 1) ||
        test4 (1, INT_MIN) || test4 (1, 2) || !test4 (2, 1)) {
        __builtin_abort();	
    }    	

    return 0;
}

/* { dg-final { scan-tree-dump-times "\\+ 4294967295;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\+ -1;" 2 "optimized" } } */
