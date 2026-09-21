/* { dg-do run } */
/* { dg-options "-O2 -fsignaling-nans -fno-inline" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>

static void t_inff (float x, bool res)
{
  if (__builtin_isinff (x) != res)
    __builtin_abort ();
  if (__builtin_isinff (-x) != res)
    __builtin_abort ();
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
}

static void t_inf (double x, bool res)
{
  if (__builtin_isinf (x) != res)
    __builtin_abort ();
  if (__builtin_isinf (-x) != res)
    __builtin_abort ();
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
}

static void t_finf (float x, bool res)
{
  if (__builtin_isfinite (x) != res)
    __builtin_abort ();
  if (__builtin_isfinite (-x) != res)
    __builtin_abort ();
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
}

static void t_fin (double x, bool res)
{
  if (__builtin_isfinite (x) != res)
    __builtin_abort ();
  if (__builtin_isfinite (-x) != res)
    __builtin_abort ();
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
}

static void t_nanf (float x, bool res)
{
  if (__builtin_isnan (x) != res)
    __builtin_abort ();
  if (__builtin_isnan (-x) != res)
    __builtin_abort ();
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
}

static void t_nan (double x, bool res)
{
  if (__builtin_isnan (x) != res)
    __builtin_abort ();
  if (__builtin_isnan (-x) != res)
    __builtin_abort ();
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
}

static void t_normalf (float x, bool res)
{
  if (__builtin_isnormal (x) != res)
    __builtin_abort ();
  if (__builtin_isnormal (-x) != res)
    __builtin_abort ();
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
}

static void t_normal (double x, bool res)
{
  if (__builtin_isnormal (x) != res)
    __builtin_abort ();
  if (__builtin_isnormal (-x) != res)
    __builtin_abort ();
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
}


/* Generate the _Float16 and __bf16 testers with a macro rather than
   spelling out the same body for each type.  */
#define DEF_TEST(NAME, TYPE, PRED)		\
static void NAME (TYPE x, bool res)		\
{						\
  if (PRED (x) != res)				\
    __builtin_abort ();				\
  if (PRED (-x) != res)				\
    __builtin_abort ();				\
  if (fetestexcept (FE_INVALID))		\
    __builtin_abort ();				\
}

DEF_TEST (t_normal16, _Float16, __builtin_isnormal)
DEF_TEST (t_normalbf, __bf16, __builtin_isnormal)

int
main ()
{
  feclearexcept (FE_INVALID);

  t_inff (0.0f, 0);
  t_inff (1.0f, 0);
  t_inff (__builtin_inff (), 1);
  t_inff (__builtin_nansf (""), 0);
  t_inff (__builtin_nanf (""), 0);

  t_inf (0.0, 0);
  t_inf (1.0, 0);
  t_inf (__builtin_inf (), 1);
  t_inf (__builtin_nans (""), 0);
  t_inf (__builtin_nan (""), 0);

  t_finf (0.0f, 1);
  t_finf (1.0f, 1);
  t_finf (__builtin_inff (), 0);
  t_finf (__builtin_nansf (""), 0);
  t_finf (__builtin_nanf (""), 0);

  t_fin (0.0, 1);
  t_fin (1.0, 1);
  t_fin (__builtin_inf (), 0);
  t_fin (__builtin_nans (""), 0);
  t_fin (__builtin_nan (""), 0);

  t_nanf (0.0f, 0);
  t_nanf (1.0f, 0);
  t_nanf (__builtin_inff (), 0);
  t_nanf (__builtin_nansf (""), 1);
  t_nanf (__builtin_nanf (""), 1);

  t_nan (0.0, 0);
  t_nan (1.0, 0);
  t_nan (__builtin_inf (), 0);
  t_nan (__builtin_nans (""), 1);
  t_nan (__builtin_nan (""), 1);

  t_normalf (0.0f, 0);
  t_normalf (1.0f, 1);
  t_normalf (__FLT_MIN__, 1);
  t_normalf (__FLT_MAX__, 1);
  t_normalf (__FLT_DENORM_MIN__, 0);
  t_normalf (__builtin_inff (), 0);
  t_normalf (__builtin_nansf (""), 0);
  t_normalf (__builtin_nanf (""), 0);

  t_normal (0.0, 0);
  t_normal (1.0, 1);
  t_normal (__DBL_MIN__, 1);
  t_normal (__DBL_MAX__, 1);
  t_normal (__DBL_DENORM_MIN__, 0);
  t_normal (__builtin_inf (), 0);
  t_normal (__builtin_nans (""), 0);
  t_normal (__builtin_nan (""), 0);

  t_normal16 (0.0f16, 0);
  t_normal16 (1.0f16, 1);
  t_normal16 (__FLT16_MIN__, 1);
  t_normal16 (__FLT16_MAX__, 1);
  t_normal16 (__FLT16_DENORM_MIN__, 0);
  t_normal16 ((_Float16) __builtin_inff (), 0);
  t_normal16 (__builtin_nansf16 (""), 0);
  t_normal16 (__builtin_nanf16 (""), 0);

  t_normalbf (0.0bf16, 0);
  t_normalbf (1.0bf16, 1);
  t_normalbf (__BFLT16_MIN__, 1);
  t_normalbf (__BFLT16_MAX__, 1);
  t_normalbf (__BFLT16_DENORM_MIN__, 0);
  t_normalbf ((__bf16) __builtin_inff (), 0);
  t_normalbf (__builtin_nansf16b (""), 0);
  t_normalbf ((__bf16) __builtin_nanf (""), 0);

  return 0;
}
