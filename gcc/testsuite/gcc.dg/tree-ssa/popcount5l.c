/* PR tree-optimization/94800 */
/* { dg-do compile { target int32plus } } */
/* { dg-require-effective-target popcountl } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#if __SIZEOF_LONG__ == 4
const unsigned long m1  = 0x55555555UL;
const unsigned long m2  = 0x33333333UL;
const unsigned long m4  = 0x0F0F0F0FUL;
const int shift = 24;
#else
const unsigned long m1  = 0x5555555555555555UL;
const unsigned long m2  = 0x3333333333333333UL;
const unsigned long m4  = 0x0f0f0f0f0f0f0f0fUL;
const int shift = 56;
#endif


int popcount64c(unsigned long x)
{
    x -= (x >> 1) & m1;
    x = (x & m2) + ((x >> 2) & m2);
    x = (x + (x >> 4)) & m4;
    x += (x << 8);
    x += (x << 16);
#if __SIZEOF_LONG__ != 4
    x += (x << 32);
#endif
    return x >> shift;
}

/* { dg-final { scan-tree-dump-times "\.POPCOUNT" 1 "optimized" } } */
