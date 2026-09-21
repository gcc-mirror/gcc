/* { dg-do run } */
/* { dg-options "-O2 -fsignaling-nans -fno-inline" } */
/* { dg-require-effective-target fenv_exceptions } */

/* With -fsignaling-nans the isnan, isinf and isnormal optabs are all
   available, so __builtin_fpclassify must be built on them rather than on
   FP comparisons, which raise FE_INVALID for a signaling NaN.  */

#include <fenv.h>

enum { NAN_, INF_, NORMAL_, SUBNORMAL_, ZERO_ };

static void t_fpclassf (float x, int res)
{
  if (__builtin_fpclassify (NAN_, INF_, NORMAL_, SUBNORMAL_, ZERO_, x) != res)
    __builtin_abort ();
  if (__builtin_fpclassify (NAN_, INF_, NORMAL_, SUBNORMAL_, ZERO_, -x) != res)
    __builtin_abort ();
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
}

static void t_fpclass (double x, int res)
{
  if (__builtin_fpclassify (NAN_, INF_, NORMAL_, SUBNORMAL_, ZERO_, x) != res)
    __builtin_abort ();
  if (__builtin_fpclassify (NAN_, INF_, NORMAL_, SUBNORMAL_, ZERO_, -x) != res)
    __builtin_abort ();
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
}

#define DEF_TEST(NAME, TYPE)						\
static void NAME (TYPE x, int res)					\
{									\
  if (__builtin_fpclassify (NAN_, INF_, NORMAL_, SUBNORMAL_, ZERO_, x)	\
      != res)								\
    __builtin_abort ();							\
  if (__builtin_fpclassify (NAN_, INF_, NORMAL_, SUBNORMAL_, ZERO_, -x)	\
      != res)								\
    __builtin_abort ();							\
  if (fetestexcept (FE_INVALID))					\
    __builtin_abort ();							\
}

DEF_TEST (t_fpclass16, _Float16)
DEF_TEST (t_fpclassbf, __bf16)

int
main ()
{
  feclearexcept (FE_INVALID);

  t_fpclassf (0.0f, ZERO_);
  t_fpclassf (1.0f, NORMAL_);
  t_fpclassf (__FLT_MIN__, NORMAL_);
  t_fpclassf (__FLT_MAX__, NORMAL_);
  t_fpclassf (__FLT_DENORM_MIN__, SUBNORMAL_);
  t_fpclassf (__builtin_inff (), INF_);
  t_fpclassf (__builtin_nansf (""), NAN_);
  t_fpclassf (__builtin_nanf (""), NAN_);

  t_fpclass (0.0, ZERO_);
  t_fpclass (1.0, NORMAL_);
  t_fpclass (__DBL_MIN__, NORMAL_);
  t_fpclass (__DBL_MAX__, NORMAL_);
  t_fpclass (__DBL_DENORM_MIN__, SUBNORMAL_);
  t_fpclass (__builtin_inf (), INF_);
  t_fpclass (__builtin_nans (""), NAN_);
  t_fpclass (__builtin_nan (""), NAN_);

  t_fpclass16 (0.0f16, ZERO_);
  t_fpclass16 (1.0f16, NORMAL_);
  t_fpclass16 (__FLT16_MIN__, NORMAL_);
  t_fpclass16 (__FLT16_MAX__, NORMAL_);
  t_fpclass16 (__FLT16_DENORM_MIN__, SUBNORMAL_);
  t_fpclass16 ((_Float16) __builtin_inff (), INF_);
  t_fpclass16 (__builtin_nansf16 (""), NAN_);
  t_fpclass16 (__builtin_nanf16 (""), NAN_);

  t_fpclassbf (0.0bf16, ZERO_);
  t_fpclassbf (1.0bf16, NORMAL_);
  t_fpclassbf (__BFLT16_MIN__, NORMAL_);
  t_fpclassbf (__BFLT16_MAX__, NORMAL_);
  t_fpclassbf (__BFLT16_DENORM_MIN__, SUBNORMAL_);
  t_fpclassbf ((__bf16) __builtin_inff (), INF_);
  t_fpclassbf (__builtin_nansf16b (""), NAN_);
  t_fpclassbf ((__bf16) __builtin_nanf (""), NAN_);

  return 0;
}
