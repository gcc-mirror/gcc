/* PR target/84786 */
/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-mavx512f -mno-avx512vl -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

typedef double V __attribute__((vector_size (16)));

__attribute__((noipa)) V
foo (V x, double y)
{
  register double z __asm ("xmm18");
  asm volatile ("" : "=v" (z) : "0" (y));
  x[1] = z;
  return x;
}

static void
avx512f_test (void)
{
  V a = foo ((V) { 1.0, 2.0 }, 3.0);
  if (a[0] != 1.0 || a[1] != 3.0)
    abort ();
}
