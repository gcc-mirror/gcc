/* PR target/116287 */
/* { dg-do run { target bmi2 } } */
/* { dg-options "-O2 -mbmi2" } */

#include <x86intrin.h>

#include "bmi2-check.h"

static void
bmi2_test ()
{
  unsigned int a = 0;
  if (__builtin_ia32_bzhi_si (a++, 0) != 0)
    abort ();
  if (a != 1)
    abort ();
#ifdef __x86_64__
  unsigned long long b = 0;
  if (__builtin_ia32_bzhi_di (b++, 0) != 0)
    abort ();
  if (b != 1)
    abort ();
#endif
}
