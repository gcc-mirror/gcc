/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -dp" } */

#include <stdint.h>

#define DO_REGREG_OPS(TYPE)                                                    \
  void __attribute__ ((noclone))                                               \
  varith_##TYPE##_reg (TYPE *__restrict x, TYPE *__restrict y, int count)      \
  {                                                                            \
    for (int i = 0; i < count; ++i)                                            \
      x[i] = x[i] < y[i] ? x[i] : y[i];                                        \
  }

#define DO_IMMEDIATE_OPS(VALUE, TYPE)                                          \
  void __attribute__ ((noclone))                                               \
  varithimm_##VALUE##_##TYPE (TYPE *__restrict x, int count)                   \
  {                                                                            \
    for (int i = 0; i < count; ++i)                                            \
      x[i] = x[i] < (TYPE) VALUE ? x[i] : (TYPE) VALUE;                        \
  }

#define DO_ARITH_OPS(REG, IMM, TYPE)                                           \
  REG (TYPE);                                                                  \
  IMM (2, TYPE);                                                               \
  IMM (86, TYPE);                                                              \
  IMM (109, TYPE);                                                             \
  IMM (141, TYPE);                                                             \
  IMM (229, TYPE);                                                             \
  IMM (255, TYPE);                                                             \
  IMM (992137445376, TYPE);

#define TEST_ALL(REG, IMM)                                                     \
  DO_ARITH_OPS (REG, IMM, uint8_t)                                             \
  DO_ARITH_OPS (REG, IMM, uint16_t)                                            \
  DO_ARITH_OPS (REG, IMM, uint32_t)                                            \
  DO_ARITH_OPS (REG, IMM, uint64_t)

TEST_ALL (DO_REGREG_OPS, DO_IMMEDIATE_OPS)

/* One per 64-bit test case: */
/* { dg-final { scan-assembler-times {vec_cmpv64didi} 8 } } */
/* { dg-final { scan-assembler-times {\tv_cmp_lt_u64\tvcc, v[[0-9]+:[0-9]+], v[[0-9]+:[0-9]+]} 8 } } */

/* Two per test case with wide-enough type:*/
/* { dg-final { scan-assembler-times {\tv_cmpx_gt_i32\tvcc, s[0-9]+, v[0-9]+} 56 } } */
