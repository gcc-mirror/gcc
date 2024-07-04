/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, int l, int n, int m, int cond)
{
  size_t vl;
  switch (cond)
  {
  case 1:
    vl = 100;
    break;
  case 2:
    vl = *(size_t*)(in + 100);
    break;
  case 3:
    {
      size_t new_vl = *(size_t*)(in + 500);
      size_t new_vl2 = *(size_t*)(in + 600);
      vl = new_vl + new_vl2 + 777;
      break;
    }
  default:
    break;
  }
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i + j, vl);
          __riscv_vse8_v_i8mf8 (out + i + j, v, vl);
        }
    }
  }
}

/* { dg-final { scan-assembler-times {\.L[0-9]+\:\s+vle8\.v\s+v[0-9]+,\s*0\s*\([a-x0-9]+\)} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
