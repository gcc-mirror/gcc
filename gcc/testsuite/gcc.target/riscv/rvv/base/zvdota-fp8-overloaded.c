/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfqwdota8f -mabi=lp64d -O2" } */

#include "riscv_vector.h"

#ifdef FIXED_RVV
typedef vfloat8e4m3m1_t fp8e4_t
  __attribute__ ((riscv_rvv_vector_bits (128)));
typedef vfloat8e5m2m1_t fp8e5_t
  __attribute__ ((riscv_rvv_vector_bits (128)));
#else
typedef vfloat8e4m3m1_t fp8e4_t;
typedef vfloat8e5m2m1_t fp8e5_t;
#endif

/* Load complete vector objects containing pre-encoded FP8 data.  The four
   format combinations must resolve to distinct overloaded intrinsics.  */
#define TEST(NAME, TYPE2, TYPE1) \
  void NAME (float *acc, const TYPE2 *a, const TYPE1 *b, size_t vl) \
  { \
    vfloat32m1_t vd = __riscv_vle32_v_f32m1 (acc, 1); \
    vd = __riscv_vfqwdota (vd, *a, *b, vl); \
    __riscv_vse32_v_f32m1 (acc, vd, 1); \
  }

TEST (test_e4_e4, fp8e4_t, fp8e4_t)
TEST (test_e4_e5, fp8e4_t, fp8e5_t)
TEST (test_e5_e4, fp8e5_t, fp8e4_t)
TEST (test_e5_e5, fp8e5_t, fp8e5_t)

/* { dg-final { scan-assembler-times {vfqwdota\.vv} 2 } } */
/* { dg-final { scan-assembler-times {vfqwdota\.alt\.vv} 2 } } */
/* { dg-final { scan-assembler-times {e8alt,\s*m1,} 2 } } */
/* { dg-final { scan-assembler {vl1re8\.v} } } */
/* { dg-final { scan-assembler-times {vse32\.v} 4 } } */
