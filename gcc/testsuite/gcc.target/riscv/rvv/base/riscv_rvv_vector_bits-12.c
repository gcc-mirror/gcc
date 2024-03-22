/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64 -mrvv-vector-bits=zvl -O3" } */

#include "riscv_rvv_vector_bits.h"

TEST_FIXED_TYPE_BOOL_ALL (vbool1_t, 128, v16qi)
TEST_FIXED_TYPE_BOOL_ALL (vbool2_t,  64,  v8qi)
TEST_FIXED_TYPE_BOOL_ALL (vbool4_t,  32,  v4qi)
TEST_FIXED_TYPE_BOOL_ALL (vbool8_t,  16,  v2qi)
TEST_FIXED_TYPE_BOOL_ALL (vbool16_t,  8,  v1qi)
TEST_FIXED_TYPE_BOOL_ALL (vbool32_t,  4,  v1qi)
TEST_FIXED_TYPE_BOOL_ALL (vbool64_t,  2,  v1qi)

/* { dg-final { scan-assembler-not {csrr\s+[atx][0-9]+,\s*vlenb} } } */
