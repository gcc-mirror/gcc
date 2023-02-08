/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */
#include "riscv_vector.h"

void f1 (void * in, void *out, int32_t x)
{
  vbool32_t mask = __riscv_vlm_v_b32 (in + 100, 4);
  vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
  vint32m1_t v2 = __riscv_vle32_v_i32m1_tu (v, in, 4);
  vint32m1_t v3 = __riscv_vadc_vxm_i32m1 (v2, -16, mask, 4);
  vint32m1_t v4 = __riscv_vadc_vxm_i32m1_tu (v3, v2, -16, mask, 4);
  __riscv_vse32_v_i32m1 (out, v4, 4);
}

void f2 (void * in, void *out, int32_t x)
{
  vbool32_t mask = __riscv_vlm_v_b32 (in + 100, 4);
  vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
  vint32m1_t v2 = __riscv_vle32_v_i32m1_tu (v, in, 4);
  vint32m1_t v3 = __riscv_vadc_vxm_i32m1 (v2, 15, mask, 4);
  vint32m1_t v4 = __riscv_vadc_vxm_i32m1_tu (v3, v2, 15, mask, 4);
  __riscv_vse32_v_i32m1 (out, v4, 4);
}

void f3 (void * in, void *out, int32_t x)
{
  vbool32_t mask = __riscv_vlm_v_b32 (in + 100, 4);
  vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
  vint32m1_t v2 = __riscv_vle32_v_i32m1_tu (v, in, 4);
  vint32m1_t v3 = __riscv_vadc_vxm_i32m1 (v2, -17, mask, 4);
  vint32m1_t v4 = __riscv_vadc_vxm_i32m1_tu (v3, v2, -17, mask, 4);
  __riscv_vse32_v_i32m1 (out, v4, 4);
}

void f4 (void * in, void *out, int32_t x)
{
  vbool32_t mask = __riscv_vlm_v_b32 (in + 100, 4);
  vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
  vint32m1_t v2 = __riscv_vle32_v_i32m1_tu (v, in, 4);
  vint32m1_t v3 = __riscv_vadc_vxm_i32m1 (v2, 16, mask, 4);
  vint32m1_t v4 = __riscv_vadc_vxm_i32m1_tu (v3, v2, 16, mask, 4);
  __riscv_vse32_v_i32m1 (out, v4, 4);
}

/* { dg-final { scan-assembler-times {vadc\.vim\s+v[0-9]+,\s*v[0-9]+,\s*-16,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vadc\.vim\s+v[0-9]+,\s*v[0-9]+,\s*15,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 4 } } */

