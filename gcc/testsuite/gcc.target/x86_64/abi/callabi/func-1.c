/* Test for cross x86_64<->w64 abi standard calls.
*/
/* Origin: Kai Tietz <kai.tietz@onevision.com> */
/* { dg-do run } */
/* { dg-options "-std=gnu99 -ffast-math" } */
#include "callabi.h"

extern void abort (void);

long double
CALLABI_CROSS func_cross (long double a, double b, float c, long d, int e,
			  char f)
{
  long double ret;
  ret = a + (long double) b + (long double) c;
  ret *= (long double) (d + (long) e);
  if (f>0)
    ret += func_cross (a,b,c,d,e,-f);
  return ret;
}

long double
CALLABI_NATIVE func_native (long double a, double b, float c, long d, int e,
			    char f)
{
  long double ret;
  ret = a + (long double) b + (long double) c;
  ret *= (long double) (d + (long) e);
  if (f>0)
    ret += func_native (a,b,c,d,e,-f);
  return ret;
}

int main ()
{
  if (func_cross (1.0,2.0,3.0,1,2,3)
      != func_native (1.0,2.0,3.0,1,2,3))
    abort ();
  return 0;
}
