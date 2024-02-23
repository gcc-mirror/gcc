/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -dp" } */
/* The 'scan-assembler' directives are specific to 64-lane vectors.
   { dg-additional-options --param=gcn-preferred-vectorization-factor=64 } */

#include <stdint.h>

#define DO_REGREG_OPS(TYPE)                                                    \
  void __attribute__ ((noclone))                                               \
  varith_##TYPE##_reg (TYPE *__restrict x, TYPE *__restrict y,                 \
		       TYPE *__restrict pred, int count)                       \
  {                                                                            \
    for (int i = 0; i < count; ++i)                                            \
      x[i] = (pred[i] != -1) ? (x[i] > y[i] ? x[i] : y[i]) : -4;               \
  }

#define DO_IMMEDIATE_OPS(VALUE, TYPE, NAME)                                    \
  void __attribute__ ((noclone))                                               \
  varithimm_##NAME##_##TYPE (TYPE *__restrict x, TYPE *__restrict pred,        \
			     int count)                                        \
  {                                                                            \
    for (int i = 0; i < count; ++i)                                            \
      x[i]                                                                     \
	= (pred[i] != -1) ? (x[i] > (TYPE) VALUE ? x[i] : (TYPE) VALUE) : -4;  \
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

/* One per test case < 64 bits */
/* { dg-final { scan-assembler-times {smaxv64si3_exec} 30 } } */
/* { dg-final { scan-assembler-not {smaxv64si3/0} } } */
/* { dg-final { scan-assembler-not {\tv_writelane_b32\tv[0-9]+, vcc_??, 0} } } */

/* Two per test case: */
/* { dg-final { scan-assembler-times {\tv_cmp_gt_i32\tvcc, s[0-9]+, v[0-9]+} 80 } } */
/* { dg-final { scan-assembler-not {\tv_cmpx_gt_i32\tvcc, s[0-9]+, v[0-9]+} } } */
/* { dg-final { scan-assembler-not {\ts_cmpk_lg_u32\tvcc_lo, 0} } } */

/* One per 64-bit test case: */
/* { dg-final { scan-assembler-times {\tv_cmp_ne_u64\ts\[[0-9]+:[0-9]+\], v\[[0-9]+:[0-9]+\], -1} 10 } } */
/* { dg-final { scan-assembler-times {\tv_cmp_gt_i64\tvcc, v[[0-9]+:[0-9]+], v[[0-9]+:[0-9]+]} 10 } } */
