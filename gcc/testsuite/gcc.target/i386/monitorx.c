/* { dg-do compile } */
/* { dg-options "-O2 -mmwaitx" } */

/* Verify that they work in both 32bit and 64bit.  */

#include <x86intrin.h>

void
foo (char *p, int x, int y, int z, int c)
{
   _mm_monitorx (p, y, x);
   _mm_mwaitx (z, y, c);
}

void
bar (char *p, long x, long y, long z, long c)
{
   _mm_monitorx (p, y, x);
   _mm_mwaitx (z, y, c);
}

void
foo1 (char *p)
{
   _mm_monitorx (p, 0, 0);
   _mm_mwaitx (0, 0, 0);
}
