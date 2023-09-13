/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/94884 */

#define bool _Bool
bool decide() __attribute((const));

inline unsigned getXOrY(unsigned x, unsigned y)
{
    return decide() ? y : x;
}

bool f(unsigned x, unsigned y)
{
    return (x | y) >= getXOrY(x, y);
}


/* { dg-final { scan-tree-dump-not " >= " "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\\| " "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1;" 1 "optimized" } } */