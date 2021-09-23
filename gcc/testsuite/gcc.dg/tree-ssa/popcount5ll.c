/* PR tree-optimization/94800 */
/* { dg-do compile } */
/* { dg-require-effective-target popcountll } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

const unsigned long long m1  = 0x5555555555555555ULL;
const unsigned long long m2  = 0x3333333333333333ULL;
const unsigned long long m4  = 0x0F0F0F0F0F0F0F0FULL;
const int shift = 56;

int popcount64c(unsigned long long x)
{
    x -= (x >> 1) & m1;
    x = (x & m2) + ((x >> 2) & m2);
    x = (x + (x >> 4)) & m4;
    x += (x << 8);
    x += (x << 16);
    x += (x << 32);
    return x >> shift;
}

/* { dg-final { scan-tree-dump-times "\.POPCOUNT" 1 "optimized" { target { lp64 } } } } */
/* { dg-final { scan-tree-dump-times "\.POPCOUNT" 2 "optimized" { target { ! lp64 } } } } */
