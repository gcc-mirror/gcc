#include <math.h>

extern void abort(void);

#define C99_MATH_TESTS(nan, inf, huge, norm, sub, zero)	\
{							\
  if (fpclassify (nan) != FP_NAN)			\
    abort ();						\
							\
  if (fpclassify (inf) != FP_INFINITE)			\
    abort ();						\
							\
  if (fpclassify (huge) != FP_INFINITE)			\
    abort ();						\
							\
  if (fpclassify (norm) != FP_NORMAL)			\
    abort ();						\
							\
  if (fpclassify (sub) != FP_SUBNORMAL)			\
    abort ();						\
							\
  if (fpclassify (zero) != FP_ZERO)			\
    abort ();						\
							\
							\
  if (!isnan (nan))					\
    abort ();						\
							\
  if (isnan (inf))					\
    abort ();						\
							\
  if (isnan (huge))					\
    abort ();						\
							\
  if (isnan (norm))					\
    abort ();						\
							\
  if (isnan (sub))					\
    abort ();						\
							\
  if (isnan (zero))					\
    abort ();						\
							\
							\
  if (isinf (nan))					\
    abort ();						\
							\
  if (!isinf (inf))					\
    abort ();						\
							\
  if (!isinf (huge))					\
    abort ();						\
							\
  if (isinf (norm))					\
    abort ();						\
							\
  if (isinf (sub))					\
    abort ();						\
							\
  if (isinf (zero))					\
    abort ();						\
							\
							\
  if (isfinite (nan))					\
    abort ();						\
							\
  if (isfinite (inf))					\
    abort ();						\
							\
  if (isfinite (huge))					\
    abort ();						\
							\
  if (!isfinite (norm))					\
    abort ();						\
							\
  if (!isfinite (sub))					\
    abort ();						\
							\
  if (!isfinite (zero))					\
    abort ();						\
							\
							\
  if (isnormal (nan))					\
    abort ();						\
							\
  if (isnormal (inf))					\
    abort ();						\
							\
  if (isnormal (huge))					\
    abort ();						\
							\
  if (!isnormal (norm))					\
    abort ();						\
							\
  if (isnormal (sub))					\
    abort ();						\
							\
  if (isnormal (zero))					\
    abort ();						\
							\
							\
  if (signbit (norm))					\
    abort ();						\
							\
  if (!signbit (-(norm)))				\
    abort ();						\
							\
							\
  if (!isgreater ((inf), (norm)))			\
    abort ();						\
							\
  if (!isgreaterequal ((inf), (huge)))			\
    abort ();						\
							\
  if (!isless ((norm), (inf)))				\
    abort ();						\
							\
  if (!islessequal ((huge), (inf)))			\
    abort ();						\
							\
  if (!islessgreater ((inf), (norm)))			\
    abort ();						\
							\
  if (!isunordered ((nan), (norm)))			\
    abort ();						\
}
