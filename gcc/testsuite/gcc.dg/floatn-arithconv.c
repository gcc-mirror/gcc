/* Tests for _FloatN / _FloatNx types: test usual arithmetic
   conversions.  */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-add-options float32 } */
/* { dg-add-options float64 } */
/* { dg-add-options float32x } */
/* { dg-require-effective-target float32 } */
/* { dg-require-effective-target float64 } */
/* { dg-require-effective-target float32x } */

#define __STDC_WANT_IEC_60559_TYPES_EXT__
#include <float.h>

int i;

#define TEST(VAR, TYPE1, TYPE2, RESTYPE)			\
  do								\
    {								\
      typedef __typeof__ ((TYPE1) 0 + (TYPE2) 1) restype;	\
      typedef __typeof__ (i ? (TYPE1) 0 : (TYPE2) 1) restype2;	\
      typedef RESTYPE exptype;					\
      extern restype VAR;					\
      extern restype2 VAR;					\
      extern exptype VAR;					\
    }								\
  while (0)

void
f (void)
{
  TEST (v1, float, double, double);
#if DBL_MANT_DIG > FLT32_MANT_DIG
  TEST (v2, double, _Float32, double);
#endif
#if DBL_MANT_DIG <= FLT64_MANT_DIG
  TEST (v3, double, _Float64, _Float64);
#endif
#if DBL_MANT_DIG >= FLT32X_MANT_DIG
  TEST (v4, double, _Float32x, double);
#endif
#if FLT_MANT_DIG <= FLT32_MANT_DIG
  TEST (v5, float, _Float32, _Float32);
#endif
#if FLT32X_MANT_DIG <= FLT64_MANT_DIG
  TEST (v6, _Float32x, _Float64, _Float64);
#endif
  TEST (v7, _Float32, _Float64, _Float64);
  TEST (v8, _Float32, _Float32x, _Float32x);
}
