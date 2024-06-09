/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define b_and(A, B) ((A) & (B))
#define b_orr(A, B) ((A) | (B))
#define b_xor(A, B) ((A) ^ (B))
#define b_nand(A, B) (!((A) & (B)))
#define b_nor(A, B) (!((A) | (B)))
#define b_xnor(A, B) (!(A) ^ (B))
#define b_andnot(A, B) ((A) & !(B))
#define b_ornot(A, B) ((A) | !(B))

#define LOOP(TYPE, BINOP)                                                      \
  void __attribute__ ((noinline, noclone))                                     \
  test_##TYPE##_##BINOP (TYPE *restrict dest, TYPE *restrict src,              \
			 TYPE *restrict a, TYPE *restrict b, TYPE *restrict c, \
			 TYPE *restrict d, TYPE fallback, int count)           \
  {                                                                            \
    for (int i = 0; i < count; ++i)                                            \
      {                                                                        \
	TYPE srcv = src[i];                                                    \
	dest[i] = (BINOP (__builtin_isunordered (a[i], b[i]),                  \
			  __builtin_isunordered (c[i], d[i]))                  \
		     ? srcv                                                    \
		     : fallback);                                              \
      }                                                                        \
  }

#define TEST_BINOP(T, BINOP)                                                   \
  T (float, BINOP)                                                             \
  T (double, BINOP)

#define TEST_ALL(T)                                                            \
  TEST_BINOP (T, b_and)                                                        \
  TEST_BINOP (T, b_orr)                                                        \
  TEST_BINOP (T, b_xor)                                                        \
  TEST_BINOP (T, b_nand)                                                       \
  TEST_BINOP (T, b_nor)                                                        \
  TEST_BINOP (T, b_xnor)                                                       \
  TEST_BINOP (T, b_andnot)                                                     \
  TEST_BINOP (T, b_ornot)

TEST_ALL (LOOP)

/* { dg-final { scan-assembler-times {\tvmand\.mm} 2 } } */
/* { dg-final { scan-assembler-times {\tvmor\.mm} 2 } } */
/* { dg-final { scan-assembler-times {\tvmxor\.mm} 2 } } */
/* { dg-final { scan-assembler-times {\tvmnot\.m} 4 } } */
/* { dg-final { scan-assembler-times {\tvmxnor\.mm} 2 } } */
/* { dg-final { scan-assembler-times {\tvmandn\.mm} 4 } } */
/* { dg-final { scan-assembler-times {\tvmorn\.mm} 4 } } */
