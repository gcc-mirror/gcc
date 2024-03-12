/* { dg-do run } */
/* { dg-options "-O -fdump-tree-optimized" } */

#include <stdbool.h>

bool __attribute__ ((noinline))
test1 (int a, int b)
{
    int tmp = (a < b) ? b : a;
    return tmp <= a;
}

bool __attribute__ ((noinline))
test2 (int a, int b)
{
    int tmp = (a < b) ? b : a;
    return tmp > a;
}

bool __attribute__ ((noinline))
test3 (int a, int b)
{
    int tmp = (a > b) ? b : a;
    return tmp >= a;
}

bool __attribute__ ((noinline))
test4 (int a, int b)
{
    int tmp = (a > b) ? b : a;
    return tmp < a;
}

int main()
{
    if (test1 (1, 2) || !test1 (2, 1) || 
        !test2 (1, 2) || test2 (2, 1) ||
        !test3 (1, 2) || test3 (2, 1) ||
        test4 (1, 2) || !test4 (2, 1)) {
        __builtin_abort();	
    }
    return 0;
}

/* Even though test[1-4] originally has MIN/MAX, those can be optimized away
   into just comparing a and b arguments. */
/* { dg-final { scan-tree-dump-times "return 0;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not { "return 1;" } "optimized" } } */
