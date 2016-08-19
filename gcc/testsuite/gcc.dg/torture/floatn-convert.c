/* Tests for _FloatN / _FloatNx types: test conversions.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float16 } */
/* { dg-add-options float32 } */
/* { dg-add-options float64 } */
/* { dg-add-options float128 } */
/* { dg-add-options float32x } */
/* { dg-add-options float64x } */
/* { dg-add-options float128x } */
/* { dg-require-effective-target float32 } */
/* { dg-require-effective-target floatn_nx_runtime } */

#define __STDC_WANT_IEC_60559_TYPES_EXT__
#include <float.h>

#ifndef FLT16_MAX
# define _Float16 _Float32
# define FLT16_MAX FLT32_MAX
# define FLT16_MANT_DIG FLT32_MANT_DIG
# define FLT16_EPSILON FLT32_EPSILON
#endif

#ifndef FLT64_MAX
# define _Float64 _Float32
# define FLT64_MAX FLT32_MAX
# define FLT64_MANT_DIG FLT32_MANT_DIG
# define FLT64_EPSILON FLT32_EPSILON
#endif

#ifndef FLT128_MAX
# define _Float128 _Float32
# define FLT128_MAX FLT32_MAX
# define FLT128_MANT_DIG FLT32_MANT_DIG
# define FLT128_EPSILON FLT32_EPSILON
#endif

#ifndef FLT32X_MAX
# define _Float32x _Float32
# define FLT32X_MAX FLT32_MAX
# define FLT32X_MANT_DIG FLT32_MANT_DIG
# define FLT32X_EPSILON FLT32_EPSILON
#endif

#ifndef FLT64X_MAX
# define _Float64x _Float32
# define FLT64X_MAX FLT32_MAX
# define FLT64X_MANT_DIG FLT32_MANT_DIG
# define FLT64X_EPSILON FLT32_EPSILON
#endif

#ifndef FLT128X_MAX
# define _Float128x _Float32
# define FLT128X_MAX FLT32_MAX
# define FLT128X_MANT_DIG FLT32_MANT_DIG
# define FLT128X_EPSILON FLT32_EPSILON
#endif

#define CONCATX(X, Y) X ## Y
#define CONCAT(X, Y) CONCATX (X, Y)

extern void exit (int);
extern void abort (void);

#define DO_TEST(TYPE1, PFX1, TYPE2, PFX2)			\
  do								\
    {								\
      volatile TYPE1 a = (TYPE1) 1 + CONCAT (PFX1, _EPSILON);	\
      volatile TYPE2 b = (TYPE2) a;				\
      volatile TYPE2 expected;					\
      if (CONCAT (PFX2, _MANT_DIG) < CONCAT (PFX1, _MANT_DIG))	\
	expected = (TYPE2) 1;					\
      else							\
	expected = (TYPE2) 1 + (TYPE2) CONCAT (PFX1, _EPSILON); \
      if (b != expected)					\
	abort ();						\
    }								\
  while (0)

#define DO_TEST1(TYPE1, PFX1)				\
  do							\
    {							\
      DO_TEST (TYPE1, PFX1, _Float16, FLT16);		\
      DO_TEST (TYPE1, PFX1, _Float32, FLT32);		\
      DO_TEST (TYPE1, PFX1, _Float64, FLT64);		\
      DO_TEST (TYPE1, PFX1, _Float128, FLT128);		\
      DO_TEST (TYPE1, PFX1, _Float32x, FLT32X);		\
      DO_TEST (TYPE1, PFX1, _Float64x, FLT64X);		\
      DO_TEST (TYPE1, PFX1, _Float128x, FLT128X);	\
    }							\
  while (0)

int
main (void)
{
  DO_TEST1 (_Float16, FLT16);
  DO_TEST1 (_Float32, FLT32);
  DO_TEST1 (_Float64, FLT64);
  DO_TEST1 (_Float128, FLT128);
  DO_TEST1 (_Float32x, FLT32X);
  DO_TEST1 (_Float64x, FLT64X);
  DO_TEST1 (_Float128x, FLT128X);
  exit (0);
}
