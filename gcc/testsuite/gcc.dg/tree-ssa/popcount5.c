/* PR tree-optimization/94800 */
/* { dg-do compile } */
/* { dg-require-effective-target popcount } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

const unsigned m1  = 0x55555555UL;
const unsigned m2  = 0x33333333UL;
const unsigned m4  = 0x0F0F0F0FUL;
const int shift = 24;

int popcount64c(unsigned x)
{
    x -= (x >> 1) & m1;
    x = (x & m2) + ((x >> 2) & m2);
    x = (x + (x >> 4)) & m4;
    x += (x << 8);
    x += (x << 16);
    return x >> shift;
}

/* { dg-final { scan-tree-dump-times "\.POPCOUNT" 1 "optimized" } } */
