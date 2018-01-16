/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define b_and(A, B) ((A) & (B))
#define b_orr(A, B) ((A) | (B))
#define b_eor(A, B) ((A) ^ (B))
#define b_nand(A, B) (!((A) & (B)))
#define b_nor(A, B) (!((A) | (B)))
#define b_bic(A, B) ((A) & !(B))
#define b_orn(A, B) ((A) | !(B))

#define LOOP(TYPE, BINOP)						\
  void __attribute__ ((noinline, noclone))				\
  test_##TYPE##_##BINOP (TYPE *restrict dest, TYPE *restrict src,	\
			 TYPE *restrict a, TYPE *restrict b,		\
			 TYPE *restrict c, TYPE *restrict d,		\
			 TYPE fallback, int count)			\
  {									\
    for (int i = 0; i < count; ++i)					\
      dest[i] = (BINOP (__builtin_isunordered (a[i], b[i]),		\
			__builtin_isunordered (c[i], d[i]))		\
		 ? src[i] : fallback);					\
  }

#define TEST_BINOP(T, BINOP) \
  T (_Float16, BINOP) \
  T (float, BINOP) \
  T (double, BINOP)

#define TEST_ALL(T) \
  TEST_BINOP (T, b_and) \
  TEST_BINOP (T, b_orr) \
  TEST_BINOP (T, b_eor) \
  TEST_BINOP (T, b_nand) \
  TEST_BINOP (T, b_nor) \
  TEST_BINOP (T, b_bic) \
  TEST_BINOP (T, b_orn)

TEST_ALL (LOOP)

/* Currently we don't manage to remove ANDs from the other loops.  */
/* { dg-final { scan-assembler-times {\tand\tp[0-9]+\.b, p[0-9]+/z, p[0-9]+\.b, p[0-9]+\.b} 3 { xfail *-*-* } } } */
/* { dg-final { scan-assembler {\tand\tp[0-9]+\.b, p[0-9]+/z, p[0-9]+\.b, p[0-9]+\.b} } } */
/* { dg-final { scan-assembler-times {\torr\tp[0-9]+\.b, p[0-9]+/z, p[0-9]+\.b, p[0-9]+\.b} 3 } } */
/* { dg-final { scan-assembler-times {\teor\tp[0-9]+\.b, p[0-9]+/z, p[0-9]+\.b, p[0-9]+\.b} 3 } } */
/* { dg-final { scan-assembler-times {\tnand\tp[0-9]+\.b, p[0-9]+/z, p[0-9]+\.b, p[0-9]+\.b} 3 } } */
/* { dg-final { scan-assembler-times {\tnor\tp[0-9]+\.b, p[0-9]+/z, p[0-9]+\.b, p[0-9]+\.b} 3 } } */
/* { dg-final { scan-assembler-times {\tbic\tp[0-9]+\.b, p[0-9]+/z, p[0-9]+\.b, p[0-9]+\.b} 3 } } */
/* { dg-final { scan-assembler-times {\torn\tp[0-9]+\.b, p[0-9]+/z, p[0-9]+\.b, p[0-9]+\.b} 3 } } */
