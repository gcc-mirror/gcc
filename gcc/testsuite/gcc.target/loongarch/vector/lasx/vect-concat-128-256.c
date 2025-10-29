/* { dg-do compile { target { loongarch64*-*-* } } } */
/* { dg-options "-mabi=lp64d -O2 -mlasx" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <lasxintrin.h>

/*
**foo1:
**	vinsgr2vr.d	(\$vr[0-9]+),\$r5,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r7,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r6,1
**	vinsgr2vr.d	(\$vr[0-9]+),\$r8,1
**	xvpermi.q	(\$xr[0-9]+),(\$xr[0-9]+),0x02
**	xvst	(\$xr[0-9]+),\$r4,0
**	jr	\$r1
*/
__m256
foo1 (__m128 x, __m128 y)
{
  return __builtin_lasx_concat_128_s (x, y);
}

/*
**foo2:
**	vinsgr2vr.d	(\$vr[0-9]+),\$r5,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r7,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r6,1
**	vinsgr2vr.d	(\$vr[0-9]+),\$r8,1
**	xvpermi.q	(\$xr[0-9]+),(\$xr[0-9]+),0x02
**	xvst	(\$xr[0-9]+),\$r4,0
**	jr	\$r1
*/
__m256d
foo2 (__m128d x, __m128d y)
{
  return __builtin_lasx_concat_128_d (x, y);
}

/*
**foo3:
**	vinsgr2vr.d	(\$vr[0-9]+),\$r5,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r7,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r6,1
**	vinsgr2vr.d	(\$vr[0-9]+),\$r8,1
**	xvpermi.q	(\$xr[0-9]+),(\$xr[0-9]+),0x02
**	xvst	(\$xr[0-9]+),\$r4,0
**	jr	\$r1
*/
__m256i
foo3 (__m128i x, __m128i y)
{
  return __builtin_lasx_concat_128 (x, y);
}

/*
**foo4:
**	vinsgr2vr.d	(\$vr[0-9]+),\$r5,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r6,1
**	xvst	(\$xr[0-9]+),\$r4,0
**	jr	\$r1
*/
__m256
foo4 (__m128 x)
{
  return __builtin_lasx_cast_128_s (x);
}

/*
**foo5:
**	vinsgr2vr.d	(\$vr[0-9]+),\$r5,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r6,1
**	xvst	(\$xr[0-9]+),\$r4,0
**	jr	\$r1
*/
__m256d
foo5 (__m128d x)
{
  return __builtin_lasx_cast_128_d (x);
}

/*
**foo6:
**	vinsgr2vr.d	(\$vr[0-9]+),\$r5,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r6,1
**	xvst	(\$xr[0-9]+),\$r4,0
**	jr	\$r1
*/
__m256i
foo6 (__m128i x)
{
  return __builtin_lasx_cast_128 (x);
}
