/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

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
  test_##TYPE1##_##TYPE2##_##CMP##_zero (TYPE1 *restrict dest,	\
					 TYPE1 *restrict src,	\
					 TYPE1 fallback,	\
					 TYPE2 *restrict a,	\
					 int count)		\
  {								\
    for (int i = 0; i < count; ++i)				\
      dest[i] = CMP (a[i], 0) ? src[i] : fallback;		\
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

/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\.0\n} 3 } } */
/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\.0\n} 3 } } */

/* 3 for ne, 3 for ueq and 3 for nueq.  */
/* { dg-final { scan-assembler-times {\tfcmne\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\.0\n} 9 } } */
/* { dg-final { scan-assembler-times {\tfcmne\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\.0\n} 9 } } */

/* 3 for olt, 3 for ult and 3 for nult.  */
/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\.0\n} 9 } } */
/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\.0\n} 9 } } */

/* 3 for ole, 3 for ule and 3 for nule.  */
/* { dg-final { scan-assembler-times {\tfcmle\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\.0\n} 9 } } */
/* { dg-final { scan-assembler-times {\tfcmle\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\.0\n} 9 } } */

/* 3 for ogt, 3 for ugt and 3 for nugt.  */
/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\.0\n} 9 } } */
/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\.0\n} 9 } } */

/* 3 for oge, 3 for uge and 3 for nuge.  */
/* { dg-final { scan-assembler-times {\tfcmge\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\.0\n} 9 } } */
/* { dg-final { scan-assembler-times {\tfcmge\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\.0\n} 9 } } */

/* { dg-final { scan-assembler-not {\tfcmuo\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\.0\n} } } */
/* { dg-final { scan-assembler-not {\tfcmuo\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\.0\n} } } */
/* 3 invocations for all 12 unordered comparisons.  */
/* { dg-final { scan-assembler-times {\tfcmuo\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 36 } } */
/* { dg-final { scan-assembler-times {\tfcmuo\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 36 } } */
