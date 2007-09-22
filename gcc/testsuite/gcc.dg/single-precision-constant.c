/* Test that double precision constants are correctly handled 
   when code is compiled with -fsingle-precision-constant */
/* Origin: Carlos O'Donell <carlos@codesourcery.com> */

/* { dg-do run } */
/* { dg-options "-fsingle-precision-constant" } */
/* { dg-add-options c99_runtime } */

#include <math.h>
#include <float.h>

#include "builtins-config.h"

int main (void)
{
  int result = 0;
  double local_DBL_MAX = DBL_MAX; 
  double local_DBL_MIN = DBL_MIN;
#ifdef HAVE_C99_RUNTIME
  if (isinf (local_DBL_MAX))
    result |= 1;
#endif
  if (local_DBL_MIN <= 0.0)
    result |= 1;
  return result;
}
