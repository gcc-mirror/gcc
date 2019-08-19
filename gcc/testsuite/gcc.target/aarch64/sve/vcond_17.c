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

#define DEF_LOOP(CMP, EXPECT_INVALID)					\
  void __attribute__ ((noinline, noclone))				\
  test_##CMP##_var (__fp16 *restrict dest, __fp16 *restrict src,	\
		    __fp16 fallback, __fp16 *restrict a,		\
		    __fp16 *restrict b, int count)			\
  {									\
    for (int i = 0; i < count; ++i)					\
      dest[i] = CMP (a[i], b[i]) ? src[i] : fallback;			\
  }									\
									\
  void __attribute__ ((noinline, noclone))				\
  test_##CMP##_zero (__fp16 *restrict dest,  __fp16 *restrict src,	\
		     __fp16 fallback, __fp16 *restrict a,		\
		     int count)						\
  {									\
    for (int i = 0; i < count; ++i)					\
      dest[i] = CMP (a[i], (__fp16) 0) ? src[i] : fallback;		\
  }									\
									\
  void __attribute__ ((noinline, noclone))				\
  test_##CMP##_sel (__fp16 *restrict dest, __fp16 if_true,		\
		    __fp16 if_false, __fp16 *restrict a,		\
		    __fp16 b, int count)				\
  {									\
    for (int i = 0; i < count; ++i)					\
      dest[i] = CMP (a[i], b) ? if_true : if_false;			\
  }

#define TEST_ALL(T)				\
  T (eq, 0)					\
  T (ne, 0)					\
  T (olt, 1)					\
  T (ole, 1)					\
  T (oge, 1)					\
  T (ogt, 1)					\
  T (ordered, 0)				\
  T (unordered, 0)				\
  T (ueq, 0)					\
  T (ult, 0)					\
  T (ule, 0)					\
  T (uge, 0)					\
  T (ugt, 0)					\
  T (nueq, 0)					\
  T (nult, 0)					\
  T (nule, 0)					\
  T (nuge, 0)					\
  T (nugt, 0)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler {\tfcmeq\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #0\.0\n} { xfail *-*-* } } } */
/* { dg-final { scan-assembler {\tfcmeq\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} } } */

/* { dg-final { scan-assembler {\tfcmne\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #0\.0\n} } } */
/* { dg-final { scan-assembler {\tfcmne\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} } } */

/* { dg-final { scan-assembler {\tfcmlt\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #0\.0\n} } } */
/* { dg-final { scan-assembler {\tfcmlt\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} } } */

/* { dg-final { scan-assembler {\tfcmle\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #0\.0\n} } } */
/* { dg-final { scan-assembler {\tfcmle\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} } } */

/* { dg-final { scan-assembler {\tfcmgt\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #0\.0\n} } } */
/* { dg-final { scan-assembler {\tfcmgt\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} } } */

/* { dg-final { scan-assembler {\tfcmge\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #0\.0\n} } } */
/* { dg-final { scan-assembler {\tfcmge\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} } } */

/* { dg-final { scan-assembler-not {\tfcmuo\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #0\.0\n} } } */
/* { dg-final { scan-assembler {\tfcmuo\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} } } */
