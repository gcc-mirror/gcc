/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */

#define REDUC(TYPE)						\
  TYPE reduc_##TYPE (TYPE *x, TYPE *y, int count)		\
  {								\
    TYPE sum = 0;						\
    for (int i = 0; i < count; ++i)				\
      sum -= x[i] * y[i];					\
    return sum;							\
  }

REDUC (float)
REDUC (double)

/* { dg-final { scan-assembler-times {\tfmls\tz[0-9]+\.s, p[0-7]/m} 1 } } */
/* { dg-final { scan-assembler-times {\tfmls\tz[0-9]+\.d, p[0-7]/m} 1 } } */
