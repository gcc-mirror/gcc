/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (int8_t * restrict in, int8_t * restrict out, int n, int cond,size_t *new_vl,size_t *new_vl2)
{
  size_t vl = 101;
  
  vint8mf8_t v = __riscv_vle8_v_i8mf8 (in, vl);
  __riscv_vse8_v_i8mf8 (out, v, vl);
  vbool64_t mask = __riscv_vlm_v_b64 (in + 100, vl);
  vint8mf8_t v2 = __riscv_vle8ff_v_i8mf8_tumu (mask, v, in + 100, new_vl, vl);
  __riscv_vse8_v_i8mf8 (out + 100, v2, *new_vl);
  v2 = __riscv_vle8ff_v_i8mf8_tumu (mask, v2, in + 200, new_vl2, vl);
  __riscv_vse8_v_i8mf8 (out + 200, v2, *new_vl2);
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*mu} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {csrr} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-not {vmv} { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
