/* { dg-do compile } */
/* { dg-options "-O2 -mno-mwait -msse3 -mgeneral-regs-only" } */

#include <x86intrin.h>

void
foo1 (unsigned int x, unsigned int y)
{
  _mm_mwait (x, y);
}

/* { dg-error "target specific option mismatch" "" { target *-*-* } 0 } */
