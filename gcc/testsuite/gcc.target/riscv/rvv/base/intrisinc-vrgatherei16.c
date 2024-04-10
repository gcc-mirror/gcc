/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64 -O3" } */

#include "riscv_vector.h"

typedef _Float16 float16_t;

vfloat16mf4_t test_vrgatherei16_vv_f16mf4(vfloat16mf4_t op1, vuint16mf4_t op2,
  size_t vl) {
  return __riscv_vrgatherei16_vv_f16mf4(op1, op2, vl);
}

vfloat16m8_t test_vrgatherei16_vv_f16m8(vfloat16m8_t op1, vuint16m8_t op2,
  size_t vl) {
  return __riscv_vrgatherei16_vv_f16m8(op1, op2, vl);
}

vfloat16mf4_t test_vrgatherei16_vv_f16mf4_m(vbool64_t mask, vfloat16mf4_t op1,
  vuint16mf4_t op2, size_t vl) {
  return __riscv_vrgatherei16_vv_f16mf4_m(mask, op1, op2, vl);
}

vfloat16m8_t test_vrgatherei16_vv_f16m8_m(vbool2_t mask, vfloat16m8_t op1,
  vuint16m8_t op2, size_t vl) {
  return __riscv_vrgatherei16_vv_f16m8_m(mask, op1, op2, vl);
}

/* { dg-final { scan-assembler-times {vrgatherei16.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
