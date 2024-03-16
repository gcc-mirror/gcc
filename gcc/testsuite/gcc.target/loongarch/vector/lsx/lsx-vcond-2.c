/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops  -mlsx" } */

#include <stdint-gcc.h>

#define eq(A, B) ((A) == (B))
#define ne(A, B) ((A) != (B))
#define olt(A, B) ((A) < (B))
#define ole(A, B) ((A) <= (B))
#define oge(A, B) ((A) >= (B))
#define ogt(A, B) ((A) > (B))
#define ordered(A, B) (!__builtin_isunordered (A, B))
#define unordered(A, B) (__builtin_isunordered (A, B))
#define ueq(A, B) (!__builtin_islessgreater (A, B))
#define ult(A, B) (__builtin_isless (A, B))
#define ule(A, B) (__builtin_islessequal (A, B))
#define uge(A, B) (__builtin_isgreaterequal (A, B))
#define ugt(A, B) (__builtin_isgreater (A, B))
#define nueq(A, B) (__builtin_islessgreater (A, B))
#define nult(A, B) (!__builtin_isless (A, B))
#define nule(A, B) (!__builtin_islessequal (A, B))
#define nuge(A, B) (!__builtin_isgreaterequal (A, B))
#define nugt(A, B) (!__builtin_isgreater (A, B))

#define TEST_LOOP(TYPE1, TYPE2, CMP)				\
  void __attribute__ ((noinline, noclone))			\
  test_##TYPE1##_##TYPE2##_##CMP##_var (TYPE1 *restrict dest,	\
					TYPE1 *restrict src,	\
					TYPE1 fallback,		\
					TYPE2 *restrict a,	\
					TYPE2 *restrict b,	\
					int count)		\
  {								\
    for (int i = 0; i < count; ++i)				\
      {\
        TYPE2 aval = a[i]; \
        TYPE2 bval = b[i]; \
        TYPE1 srcval = src[i]; \
        dest[i] = CMP (aval, bval) ? srcval : fallback;		\
      }\
  }

#define TEST_CMP(CMP) \
  TEST_LOOP (int32_t, float, CMP) \
  TEST_LOOP (uint32_t, float, CMP) \
  TEST_LOOP (float, float, CMP) \
  TEST_LOOP (int64_t, double, CMP) \
  TEST_LOOP (uint64_t, double, CMP) \
  TEST_LOOP (double, double, CMP)

TEST_CMP (eq)
TEST_CMP (ne)
TEST_CMP (olt)
TEST_CMP (ole)
TEST_CMP (oge)
TEST_CMP (ogt)
TEST_CMP (ordered)
TEST_CMP (unordered)
TEST_CMP (ueq)
TEST_CMP (ult)
TEST_CMP (ule)
TEST_CMP (uge)
TEST_CMP (ugt)
TEST_CMP (nueq)
TEST_CMP (nult)
TEST_CMP (nule)
TEST_CMP (nuge)
TEST_CMP (nugt)

/* { dg-final { scan-assembler-times {\tvfcmp\.ceq\.s} 2 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.ceq\.d} 2 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.cne\.s} 2 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.cne\.d} 2 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.slt\.s} 4 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.slt\.d} 4 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.sle\.s} 4 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.sle\.d} 4 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.cor\.s} 2 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.cor\.d} 2 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.cun\.s} 2 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.cun\.d} 2 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.cueq\.s} 4 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.cueq\.d} 4 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.cule\.s} 8 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.cule\.d} 8 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.cult\.s} 8 } } */
/* { dg-final { scan-assembler-times {\tvfcmp\.cult\.d} 8 } } */
