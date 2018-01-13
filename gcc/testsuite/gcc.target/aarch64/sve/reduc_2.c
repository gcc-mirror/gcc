/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */

#include <stdint.h>

#define NUM_ELEMS(TYPE) (1024 / sizeof (TYPE))

#define DEF_REDUC_PLUS(TYPE)					\
void __attribute__ ((noinline, noclone))			\
reduc_plus_##TYPE (TYPE (*restrict a)[NUM_ELEMS (TYPE)],	\
		   TYPE *restrict r, int n)			\
{								\
  for (int i = 0; i < n; i++)					\
    {								\
      r[i] = 0;							\
      for (int j = 0; j < NUM_ELEMS (TYPE); j++)		\
        r[i] += a[i][j];					\
    }								\
}

#define TEST_PLUS(T)				\
  T (int8_t)					\
  T (int16_t)					\
  T (int32_t)					\
  T (int64_t)					\
  T (uint8_t)					\
  T (uint16_t)					\
  T (uint32_t)					\
  T (uint64_t)					\
  T (_Float16)					\
  T (float)					\
  T (double)

TEST_PLUS (DEF_REDUC_PLUS)

#define DEF_REDUC_MAXMIN(TYPE, NAME, CMP_OP)			\
void __attribute__ ((noinline, noclone))			\
reduc_##NAME##_##TYPE (TYPE (*restrict a)[NUM_ELEMS (TYPE)],	\
		       TYPE *restrict r, int n)			\
{								\
  for (int i = 0; i < n; i++)					\
    {								\
      r[i] = a[i][0];						\
      for (int j = 0; j < NUM_ELEMS (TYPE); j++)		\
        r[i] = a[i][j] CMP_OP r[i] ? a[i][j] : r[i];		\
    }								\
}

#define TEST_MAXMIN(T)				\
  T (int8_t, max, >)				\
  T (int16_t, max, >)				\
  T (int32_t, max, >)				\
  T (int64_t, max, >)				\
  T (uint8_t, max, >)				\
  T (uint16_t, max, >)				\
  T (uint32_t, max, >)				\
  T (uint64_t, max, >)				\
  T (_Float16, max, >)				\
  T (float, max, >)				\
  T (double, max, >)				\
						\
  T (int8_t, min, <)				\
  T (int16_t, min, <)				\
  T (int32_t, min, <)				\
  T (int64_t, min, <)				\
  T (uint8_t, min, <)				\
  T (uint16_t, min, <)				\
  T (uint32_t, min, <)				\
  T (uint64_t, min, <)				\
  T (_Float16, min, <)				\
  T (float, min, <)				\
  T (double, min, <)

TEST_MAXMIN (DEF_REDUC_MAXMIN)

/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuaddv\td[0-9]+, p[0-7], z[0-9]+\.d\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfaddv\th[0-9]+, p[0-7], z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfaddv\ts[0-9]+, p[0-7], z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfaddv\td[0-9]+, p[0-7], z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tsmaxv\tb[0-9]+, p[0-7], z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmaxv\th[0-9]+, p[0-7], z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmaxv\ts[0-9]+, p[0-7], z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmaxv\td[0-9]+, p[0-7], z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumaxv\tb[0-9]+, p[0-7], z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumaxv\th[0-9]+, p[0-7], z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumaxv\ts[0-9]+, p[0-7], z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tumaxv\td[0-9]+, p[0-7], z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnmv\th[0-9]+, p[0-7], z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnmv\ts[0-9]+, p[0-7], z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnmv\td[0-9]+, p[0-7], z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tsminv\tb[0-9]+, p[0-7], z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsminv\th[0-9]+, p[0-7], z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsminv\ts[0-9]+, p[0-7], z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsminv\td[0-9]+, p[0-7], z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuminv\tb[0-9]+, p[0-7], z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuminv\th[0-9]+, p[0-7], z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuminv\ts[0-9]+, p[0-7], z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuminv\td[0-9]+, p[0-7], z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfminnmv\th[0-9]+, p[0-7], z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfminnmv\ts[0-9]+, p[0-7], z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfminnmv\td[0-9]+, p[0-7], z[0-9]+\.d\n} 1 } } */
