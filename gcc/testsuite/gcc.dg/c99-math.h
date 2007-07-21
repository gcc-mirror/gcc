#include <math.h>
#include <fenv.h>

extern void abort(void);

#define C99_MATH_TESTS(nan, inf, huge, norm1, norm2, norm3, sub, zero, neg) \
{							\
  if (feclearexcept (FE_ALL_EXCEPT) != 0)		\
    abort();						\
							\
							\
  if (fpclassify (nan) != FP_NAN)			\
    abort ();						\
							\
  if (fpclassify (inf) != FP_INFINITE)			\
    abort ();						\
							\
  if (fpclassify (huge) != FP_INFINITE)			\
    abort ();						\
							\
  if (fpclassify (norm1) != FP_NORMAL)			\
    abort ();						\
							\
  if (fpclassify (norm2) != FP_NORMAL)			\
    abort ();						\
							\
  if (fpclassify (norm3) != FP_NORMAL)			\
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
  if (isnan (norm1))					\
    abort ();						\
							\
  if (isnan (norm2))					\
    abort ();						\
							\
  if (isnan (norm3))					\
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
  if (isinf (norm1))					\
    abort ();						\
							\
  if (isinf (norm2))					\
    abort ();						\
							\
  if (isinf (norm3))					\
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
  if (!isfinite (norm1))				\
    abort ();						\
							\
  if (!isfinite (norm2))				\
    abort ();						\
							\
  if (!isfinite (norm3))				\
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
  if (!isnormal (norm1))				\
    abort ();						\
							\
  if (!isnormal (norm2))				\
    abort ();						\
							\
  if (!isnormal (norm3))				\
    abort ();						\
							\
  if (isnormal (sub))					\
    abort ();						\
							\
  if (isnormal (zero))					\
    abort ();						\
							\
							\
  if (!!signbit (nan) != neg)				\
    abort ();						\
							\
  if (!!signbit (inf) != neg)				\
    abort ();						\
							\
  if (!!signbit (huge) != neg)				\
    abort ();						\
							\
  if (!!signbit (norm1) != neg)				\
    abort ();						\
							\
  if (!!signbit (norm2) != neg)				\
    abort ();						\
							\
  if (!!signbit (norm3) != neg)				\
    abort ();						\
							\
  if (!!signbit (sub) != neg)				\
    abort ();						\
							\
  if (!!signbit (zero) != neg)				\
    abort ();						\
							\
							\
  if (neg)						\
  {							\
    if (!isless ((inf), (norm1)))			\
      abort ();						\
							\
    if (!isless ((inf), (norm2)))			\
      abort ();						\
							\
    if (!isless ((inf), (norm3)))			\
      abort ();						\
							\
    if (!islessequal ((inf), (huge)))			\
      abort ();						\
							\
    if (!isgreater ((norm1), (inf)))			\
      abort ();						\
							\
    if (!isgreater ((norm2), (inf)))			\
      abort ();						\
							\
    if (!isgreater ((norm3), (inf)))			\
      abort ();						\
							\
    if (!isgreaterequal ((huge), (inf)))		\
      abort ();						\
  }							\
  else							\
  {							\
    if (!isgreater ((inf), (norm1)))			\
      abort ();						\
							\
    if (!isgreater ((inf), (norm2)))			\
      abort ();						\
							\
    if (!isgreater ((inf), (norm3)))			\
      abort ();						\
							\
    if (!isgreaterequal ((inf), (huge)))		\
      abort ();						\
							\
    if (!isless ((norm1), (inf)))			\
      abort ();						\
							\
    if (!isless ((norm2), (inf)))			\
      abort ();						\
							\
    if (!isless ((norm3), (inf)))			\
      abort ();						\
							\
    if (!islessequal ((huge), (inf)))			\
      abort ();						\
  }							\
							\
  if (!islessgreater ((inf), (norm1)))			\
    abort ();						\
							\
  if (!islessgreater ((inf), (norm2)))			\
    abort ();						\
							\
  if (!islessgreater ((inf), (norm3)))			\
    abort ();						\
							\
  if (!isunordered ((nan), (norm1)))			\
    abort ();						\
							\
  if (!isunordered ((nan), (norm2)))			\
    abort ();						\
							\
  if (!isunordered ((nan), (norm3)))			\
    abort ();						\
							\
							\
  if (fetestexcept (FE_ALL_EXCEPT) != 0)		\
    abort();						\
}
