/* { dg-do compile { target { loongarch64*-*-* } } } */
/* { dg-options "-mabi=lp64d -O2 -mlasx" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <lasxintrin.h>

/*
**foo1:
**	vinsgr2vr.d	(\$vr[0-9]+),\$r6,0
**	xvld	(\$xr[0-9]+),\$r5,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r7,1
**	xvpermi.q	(\$xr[0-9]+),(\$xr[0-9]+),0x30
**	xvst	(\$xr[0-9]+),\$r4,0
**	jr	\$r1
*/
__m256
foo1 (__m256 x, __m128 y)
{
  return __builtin_lasx_insert_128_lo_s (x, y);
}

/*
**foo2:
**	vinsgr2vr.d	(\$vr[0-9]+),\$r6,0
**	xvld	(\$xr[0-9]+),\$r5,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r7,1
**	xvpermi.q	(\$xr[0-9]+),(\$xr[0-9]+),0x02
**	xvst	(\$xr[0-9]+),\$r4,0
**	jr	\$r1
*/
__m256
foo2 (__m256 x, __m128 y)
{
  return __builtin_lasx_insert_128_hi_s (x, y);
}

/*
**foo3:
**	vinsgr2vr.d	(\$vr[0-9]+),\$r6,0
**	xvld	(\$xr[0-9]+),\$r5,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r7,1
**	xvpermi.q	(\$xr[0-9]+),(\$xr[0-9]+),0x30
**	xvst	(\$xr[0-9]+),\$r4,0
**	jr	\$r1
*/
__m256d
foo3 (__m256d x, __m128d y)
{
  return __builtin_lasx_insert_128_lo_d (x, y);
}

/*
**foo4:
**	vinsgr2vr.d	(\$vr[0-9]+),\$r6,0
**	xvld	(\$xr[0-9]+),\$r5,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r7,1
**	xvpermi.q	(\$xr[0-9]+),(\$xr[0-9]+),0x02
**	xvst	(\$xr[0-9]+),\$r4,0
**	jr	\$r1
*/
__m256d
foo4 (__m256d x, __m128d y)
{
  return __builtin_lasx_insert_128_hi_d (x, y);
}

/*
**foo5:
**	vinsgr2vr.d	(\$vr[0-9]+),\$r6,0
**	xvld	(\$xr[0-9]+),\$r5,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r7,1
**	xvpermi.q	(\$xr[0-9]+),(\$xr[0-9]+),0x30
**	xvst	(\$xr[0-9]+),\$r4,0
**	jr	\$r1
*/
__m256i
foo5 (__m256i x, __m128i y)
{
  return __builtin_lasx_insert_128_lo (x, y);
}

/*
**foo6:
**	vinsgr2vr.d	(\$vr[0-9]+),\$r6,0
**	xvld	(\$xr[0-9]+),\$r5,0
**	vinsgr2vr.d	(\$vr[0-9]+),\$r7,1
**	xvpermi.q	(\$xr[0-9]+),(\$xr[0-9]+),0x02
**	xvst	(\$xr[0-9]+),\$r4,0
**	jr	\$r1
*/
__m256i
foo6 (__m256i x, __m128i y)
{
  return __builtin_lasx_insert_128_hi (x, y);
}
