/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-options "-mlong-double-128" } */

/* Check if adding a qNAN and a normal long double does not generate a
   inexact exception.  */

#define _GNU_SOURCE
#include <fenv.h>

int main(void)
{
  double x = __builtin_nan ("");
  long double y = 1.1L;

  feenableexcept (FE_INEXACT);
  feclearexcept (FE_ALL_EXCEPT);
  x = x + y;
  return fetestexcept(FE_INEXACT);
}
