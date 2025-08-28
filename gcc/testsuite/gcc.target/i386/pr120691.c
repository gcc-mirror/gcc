/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O0 -mfpmath=sse" } */
/* { dg-require-effective-target fenv } */
/* { dg-require-effective-target dfp } */

#include <fenv.h>

int   main()   {
  fesetround( FE_UPWARD );
  _Decimal128   x1 =  9825,  x2 =  10000 ;

  double c = (double) (x1 / x2);

  if (c != 0.9825)
    __builtin_abort ();

  return   0 ;
}
