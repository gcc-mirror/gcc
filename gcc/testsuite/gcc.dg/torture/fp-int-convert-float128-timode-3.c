/* Test for correct rounding of conversions from __int128 to
   __float128.  */
/* { dg-do run } */
/* { dg-require-effective-target __float128 } */
/* { dg-require-effective-target base_quadfloat_support } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target fenv } */
/* { dg-options "-frounding-math" } */
/* { dg-add-options __float128 } */

#include <fenv.h>
#include <stdlib.h>

int
main (void)
{
  volatile unsigned long long h = -1ULL;
  volatile unsigned __int128 u128 = (((unsigned __int128) h) << 64) | h;
  volatile __int128 s128 = u128 >> 1;
  fesetround (FE_TOWARDZERO);
  __float128 ru = u128, rs = s128;
  if (ru != 0x1.ffffffffffffffffffffffffffffp127q)
    abort ();
  if (rs != 0x1.ffffffffffffffffffffffffffffp126q)
    abort ();
  exit (0);
}
