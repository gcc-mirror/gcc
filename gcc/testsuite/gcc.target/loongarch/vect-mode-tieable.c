/* { dg-do compile { target { loongarch64*-*-* } } } */
/* { dg-options "-mabi=lp64d -O2 -mlasx" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <lasxintrin.h>

/*
**foo1:
**	vinsgr2vr.d	(\$vr[0-9]+),\$r5,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r6,1
**	xvadd.d	(\$xr[0-9]+),(\$xr[0-9]+),(\$xr[0-9]+)
**	xvst	(\$xr[0-9]+),\$r4,0
**	jr	\$r1
*/
__m256i
foo1 (__m128i a)
{
  return __lasx_xvadd_d (__lasx_cast_128 (a), __lasx_cast_128 (a));
}

/*
**foo2:
**	vinsgr2vr.d	(\$vr[0-9]+),\$r5,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r6,1
**	xvfadd.s	(\$xr[0-9]+),(\$xr[0-9]+),(\$xr[0-9]+)
**	xvst	(\$xr[0-9]+),\$r4,0
**	jr	\$r1
*/
__m256
foo2 (__m128 a)
{
  return __lasx_xvfadd_s (__lasx_cast_128_s (a), __lasx_cast_128_s (a));
}

/*
**foo3:
**	vinsgr2vr.d	(\$vr[0-9]+),\$r5,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r6,1
**	xvfadd.d	(\$xr[0-9]+),(\$xr[0-9]+),(\$xr[0-9]+)
**	xvst	(\$xr[0-9]+),\$r4,0
**	jr	\$r1
*/
__m256d
foo3 (__m128d a)
{
  return __lasx_xvfadd_d (__lasx_cast_128_d (a), __lasx_cast_128_d (a));
}
