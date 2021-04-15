/* { dg-do compile } */
/* { dg-options "-O2 -mmwait -mgeneral-regs-only" } */

/* Verify that they work in both 32bit and 64bit.  */

#include <x86gprintrin.h>

void
foo (char *p, int x, int y, int z)
{
   _mm_monitor (p, y, x);
   _mm_mwait (z, y);
}

void
bar (char *p, long x, long y, long z)
{
   _mm_monitor (p, y, x);
   _mm_mwait (z, y);
}

void
foo1 (char *p)
{
   _mm_monitor (p, 0, 0);
   _mm_mwait (0, 0);
}
