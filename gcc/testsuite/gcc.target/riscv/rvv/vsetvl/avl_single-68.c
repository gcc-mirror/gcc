/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f2 (void * restrict in, void * restrict out, int l, int n, int m)
{
  size_t vl = 101;
  for (int i = 0; i < l; i++){
    size_t vl = i + vl + 44;
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i + j + k + 10000, vl);
          v = __riscv_vle8_v_i8mf8_tu (v, in + i + j + k + 20000, vl);
          __riscv_vse8_v_i8mf8 (out + i + j + k + 20000, v, vl);
        }
    }
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i, vl);
    __riscv_vse8_v_i8mf8 (out + i, v, vl);
  }
}

/* The second check is XFAILed because we currently don't lift
   vsetvls into non-transparent (in LCM parlance) blocks.
   See PR119547.
   In this test it is still possible because the conflicting
   register only ever feeds vsetvls.  */

/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]} 2 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } xfail { *-*-* } } } } */
/* { dg-final { scan-assembler-times {addi\s+[a-x0-9]+,\s*[a-x0-9]+,\s*44} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
