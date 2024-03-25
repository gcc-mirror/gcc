/* PR target/112526 */
/* { dg-do run { target { bmi2 && int128 } } } */
/* { dg-options "-O2 -mbmi2" } */

#include "bmi2-check.h"

__attribute__((noipa)) void
foo (unsigned long x, unsigned __int128 *y, unsigned long z, unsigned long *w)
{
  register unsigned long a __asm ("%r10") = x + z;
  register unsigned __int128 b __asm ("%r8") = ((unsigned __int128) a) * 257342423UL;
  asm volatile ("" : "+r" (b));
  asm volatile ("" : "+d" (a));
  *y = b;
  *w = a;
}

static void
bmi2_test ()
{
  unsigned __int128 y;
  unsigned long w;
  foo (10268318293806702989UL, &y, 4702524958196331333UL, &w);
  if (y != ((((unsigned __int128) 0xc72d2c9UL) << 64) | 0x9586adfdc95b225eUL)
      || w != 14970843252003034322UL)
    abort ();
}
