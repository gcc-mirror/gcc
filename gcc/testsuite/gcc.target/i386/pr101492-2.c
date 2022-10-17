/* { dg-do compile } */
/* { dg-options "-O2 -msse4.2 -mgeneral-regs-only" } */

#include <x86intrin.h>

unsigned int
foo1 (unsigned int x)
{
  return _mm_popcnt_u32 (x);
}
