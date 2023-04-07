/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -dp" } */

#include <stdint.h>

#define DO_REGREG_OPS(TYPE)                                                    \
  void __attribute__ ((noclone))                                               \
  varith_##TYPE##_reg (TYPE *__restrict x, TYPE *__restrict y, int count)      \
  {                                                                            \
    for (int i = 0; i < count; ++i)                                            \
      x[i] = x[i] > y[i] ? x[i] : y[i];                                        \
  }

#define DO_IMMEDIATE_OPS(VALUE, TYPE, NAME)                                    \
  void __attribute__ ((noclone))                                               \
  varithimm_##NAME##_##TYPE (TYPE *__restrict x, int count)                    \
  {                                                                            \
    for (int i = 0; i < count; ++i)                                            \
      x[i] = x[i] > (TYPE) VALUE ? x[i] : (TYPE) VALUE;                        \
  }

#define DO_ARITH_OPS(REG, IMM, TYPE)                                           \
  REG (TYPE);                                                                  \
  IMM (0, TYPE, 0);                                                            \
  IMM (86, TYPE, 86);                                                          \
  IMM (109, TYPE, 109);                                                        \
  IMM (141, TYPE, 141);                                                        \
  IMM (92137445376, TYPE, 92137445376);                                        \
  IMM (-1, TYPE, minus1);                                                      \
  IMM (-110, TYPE, minus110);                                                  \
  IMM (-141, TYPE, minus141);                                                  \
  IMM (-92137445376, TYPE, minus92137445376);

#define TEST_ALL(REG, IMM)                                                     \
  DO_ARITH_OPS (REG, IMM, int8_t)                                              \
  DO_ARITH_OPS (REG, IMM, int16_t)                                             \
  DO_ARITH_OPS (REG, IMM, int32_t)                                             \
  DO_ARITH_OPS (REG, IMM, int64_t)

TEST_ALL (DO_REGREG_OPS, DO_IMMEDIATE_OPS)

/* One per 64-bit test case: */
/* { dg-final { scan-assembler-times {vec_cmpv64didi} 10 } } */

/* Two per test case: */
/* { dg-final { scan-assembler-times {\tv_cmp_gt_i64\tvcc, v[[0-9]+:[0-9]+], v[[0-9]+:[0-9]+]} 10 } } */
/* { dg-final { scan-assembler-times {\tv_cmpx_gt_i32\tvcc, s[0-9]+, v[0-9]+} 80 } } */