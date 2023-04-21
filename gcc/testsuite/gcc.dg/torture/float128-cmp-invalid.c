/* Test for "invalid" exceptions from __float128 comparisons.  */
/* { dg-do run } */
/* { dg-xfail-run-if "xfail for ppc float128_hw" { ppc_float128_hw || { ppc_cpu_supports_hw && p9vector_hw } } } */
/* { dg-options "" } */
/* { dg-require-effective-target __float128 } */
/* { dg-require-effective-target base_quadfloat_support } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-add-options __float128 } */

#include <fenv.h>
#include <stdlib.h>

int
main (void)
{
  volatile __float128 a = __builtin_nanq (""), b = 0.0q;
  volatile int r = a < b;
  if (!fetestexcept (FE_INVALID))
    abort ();
  if (r)
    abort ();
  exit (0);
}
