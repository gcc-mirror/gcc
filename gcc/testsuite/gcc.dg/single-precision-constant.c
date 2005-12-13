/* Test that double precision constants are correctly handled 
   when code is compiled with -fsingle-precision-constant */
/* Origin: Carlos O'Donell <carlos@codesourcery.com> */
/* { dg-do run } */
/* { dg-options "-fsingle-precision-constant" } */
#include <math.h>
#include <float.h>

int main (void)
{
  int result = 0;
  double local_DBL_MAX = DBL_MAX; 
  double local_DBL_MIN = DBL_MIN;
  if (isinf (local_DBL_MAX))
    result |= 1;
  if (local_DBL_MIN <= 0.0)
    result |= 1;
  return result;
}
