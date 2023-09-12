/* { dg-do run } */
/* { dg-options "-O -fdump-tree-optimized" } */

#include <stdbool.h>

bool __attribute__ ((noinline))
test1(int a, int b)
{
    int tmp = (a < b) ? b : a;
    return tmp >= a;
}

bool __attribute__ ((noinline))
test2(int a, int b)
{
    int tmp = (a < b) ? b : a;
    return tmp < a;
}

bool __attribute__ ((noinline))
test3(int a, int b)
{
    int tmp = (a > b) ? b : a;
    return tmp <= a;
}

bool __attribute__ ((noinline))
test4(int a, int b)
{
    int tmp = (a > b) ? b : a;
    return tmp > a;
}

int main()
{
    if (!test1 (1, 2) || !test1 (2, 1) || 
        test2 (1, 2) || test2 (2, 1) ||
        !test3 (1, 2) || !test3 (2, 1) ||
        test4 (1, 2) || test4 (2, 1)) {
        __builtin_abort();	
    }
    return 0;
}

/* Note main has one `return 0`. */
/* { dg-final { scan-tree-dump-times "return 0;" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not { "MAX_EXPR" } "optimized" } } */
/* { dg-final { scan-tree-dump-not { "MIN_EXPR" } "optimized" } } */
