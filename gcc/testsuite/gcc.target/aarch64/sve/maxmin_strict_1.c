/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <math.h>

#define NUM_ELEMS(TYPE) (320 / sizeof (TYPE))

#define DEF_MAXMIN(TYPE, FUN)					\
void __attribute__ ((noinline, noclone))			\
test_##FUN##_##TYPE (TYPE *restrict r, TYPE *restrict a,	\
		     TYPE *restrict b)				\
{								\
  for (int i = 0; i < NUM_ELEMS (TYPE); i++)			\
    r[i] = FUN (a[i], b[i]);					\
}

#define TEST_ALL(T)				\
  T (float, fmaxf)				\
  T (double, fmax)				\
						\
  T (float, fminf)				\
  T (double, fmin)

TEST_ALL (DEF_MAXMIN)

/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfminnm\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfminnm\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
