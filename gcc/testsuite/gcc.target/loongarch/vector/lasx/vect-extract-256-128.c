/* { dg-do compile { target { loongarch64*-*-* } } } */
/* { dg-options "-mabi=lp64d -O2 -mlasx" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <lasxintrin.h>

/*
**foo1_lo:
**	xvld	(\$xr[0-9]+),\$r4,0
**	vpickve2gr.du	\$r4,(\$vr[0-9]+),0
**	vpickve2gr.du	\$r5,(\$vr[0-9]+),1
**	jr	\$r1
*/
__m128
foo1_lo (__m256 x)
{
  return __lasx_extract_128_lo_s (x);
}

/*
**foo1_hi:
**	xvld	(\$xr[0-9]+),\$r4,0
**	xvpermi.d	(\$xr[0-9]+),(\$xr[0-9]+),0xe
**	vpickve2gr.du	\$r4,(\$vr[0-9]+),0
**	vpickve2gr.du	\$r5,(\$vr[0-9]+),1
**	jr	\$r1
*/
__m128
foo1_hi (__m256 x)
{
  return __lasx_extract_128_hi_s (x);
}

/*
**foo2_lo:
**	xvld	(\$xr[0-9]+),\$r4,0
**	vpickve2gr.du	\$r4,(\$vr[0-9]+),0
**	vpickve2gr.du	\$r5,(\$vr[0-9]+),1
**	jr	\$r1
*/
__m128d
foo2_lo (__m256d x)
{
  return __lasx_extract_128_lo_d (x);
}

/*
**foo2_hi:
**	xvld	(\$xr[0-9]+),\$r4,0
**	xvpermi.d	(\$xr[0-9]+),(\$xr[0-9]+),0xe
**	vpickve2gr.du	\$r4,(\$vr[0-9]+),0
**	vpickve2gr.du	\$r5,(\$vr[0-9]+),1
**	jr	\$r1
*/    
__m128d
foo2_hi (__m256d x)
{
  return __lasx_extract_128_hi_d (x);
}

/*
**foo3_lo:
**	xvld	(\$xr[0-9]+),\$r4,0
**	vpickve2gr.du	\$r4,(\$vr[0-9]+),0
**	vpickve2gr.du	\$r5,(\$vr[0-9]+),1
**	jr	\$r1
*/
__m128i
foo3_lo (__m256i x)
{
  return __lasx_extract_128_lo (x);
}

/*
**foo3_hi:
**	xvld	(\$xr[0-9]+),\$r4,0
**	xvpermi.d	(\$xr[0-9]+),(\$xr[0-9]+),0xe
**	vpickve2gr.du	\$r4,(\$vr[0-9]+),0
**	vpickve2gr.du	\$r5,(\$vr[0-9]+),1
**	jr	\$r1
*/
__m128i
foo3_hi (__m256i x)
{
  return __lasx_extract_128_hi (x);
}
