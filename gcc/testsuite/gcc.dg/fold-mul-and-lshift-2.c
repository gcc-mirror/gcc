/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump { >> 15;} "optimized" } } */
/* { dg-final { scan-tree-dump { \* 32768;} "optimized" } } */

unsigned int
f1 (unsigned int x)
{
    x >>= 15;
    short y = x;
    y &= -2;
    x = y;
    x *= 1 << 15;
    return x;
}
