/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -dp" } */
/* The 'scan-assembler' directives are specific to 64-lane vectors.
   { dg-additional-options --param=gcn-preferred-vectorization-factor=64 } */

#include <stdint.h>

#define DEF_LOOP(TYPE, NAME, OP)                                               \
  void __attribute__ ((noipa))                                                 \
  test_##TYPE##_##NAME (TYPE *__restrict r, TYPE *__restrict a,                \
			TYPE *__restrict b, int n)                             \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      r[i] = a[i] > 20 ? b[i] OP 3 : 72;                                       \
  }

#define TEST_TYPE(T, TYPE)                                                     \
  T (TYPE, shl, <<)                                                            \
  T (TYPE, shr, >>)

#define TEST_ALL(T)                                                            \
  TEST_TYPE (T, int32_t)                                                       \
  TEST_TYPE (T, uint32_t)                                                      \
  TEST_TYPE (T, int64_t)                                                       \
  TEST_TYPE (T, uint64_t)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tv_lshlrev_b32\tv[0-9]+, 3, v[0-9]+} 10 } } */
/* { dg-final { scan-assembler-times {\tv_ashrrev_i32\tv[0-9]+, 3, v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vashlv64si3_exec} 18 } } */
/* { dg-final { scan-assembler-times {vashrv64si3_exec} 1 } } */
/* { dg-final { scan-assembler-times {vashlv64di3_exec} 2 } } */
/* { dg-final { scan-assembler-times {vashrv64di3_exec} 1 } } */
/* { dg-final { scan-assembler-times {vlshrv64si3_exec} 1 } } */
/* { dg-final { scan-assembler-times {vlshrv64di3_exec} 1 } } */

/* { dg-final { scan-assembler-not {v_cndmask_b32} } } */
/* { dg-final { scan-assembler-not {movv64di_exec/2} } } */
