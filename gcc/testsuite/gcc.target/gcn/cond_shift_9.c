/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -dp" } */

#include <stdint.h>

#define DEF_LOOP(TYPE, NAME, OP)					\
  void __attribute__ ((noipa))						\
  test_##TYPE##_##NAME (TYPE *__restrict r, TYPE *__restrict a,		\
			TYPE *__restrict b, TYPE *__restrict c, int n)	\
  {									\
    for (int i = 0; i < n; ++i)						\
      r[i] = a[i] > 20 ? b[i] OP c[i] : 0;				\
  }

#define TEST_TYPE(T, TYPE) \
  T (TYPE, shl, <<) \
  T (TYPE, shr, >>)

#define TEST_ALL(T) \
  TEST_TYPE (T, int32_t) \
  TEST_TYPE (T, uint32_t) \
  TEST_TYPE (T, int64_t) \
  TEST_TYPE (T, uint64_t)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {vashlv64si3_exec} 18 } } */
/* { dg-final { scan-assembler-times {vashrv64si3_exec} 1 } } */
/* { dg-final { scan-assembler-times {vashlv64di3_exec} 2 } } */
/* { dg-final { scan-assembler-times {vashrv64di3_exec} 1 } } */
/* { dg-final { scan-assembler-times {vlshrv64si3_exec} 1 } } */
/* { dg-final { scan-assembler-times {vlshrv64di3_exec} 1 } } */

/* { dg-final { scan-assembler-not {v_cndmask_b32} } } */
/* { dg-final { scan-assembler-not {movv64si_exec/2} } } */
/* { dg-final { scan-assembler-not {movv64di_exec/1} } } */
