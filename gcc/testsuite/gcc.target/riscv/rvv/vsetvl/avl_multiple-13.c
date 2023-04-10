/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, int n, int cond)
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
    vl = vl + 4000;
    break;
  }
  for (size_t i = 0; i < n; i++)
    {
      vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i, vl);
      __riscv_vse8_v_i8mf8 (out + i, v, vl);
      
      vint8mf8_t v2 = __riscv_vle8_v_i8mf8_tu (v, in + i + 100, vl);
      __riscv_vse8_v_i8mf8 (out + i + 100, v2, vl);
    }
  
  for (size_t i = 0; i < n; i++)
    {
      vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i + 300, vl);
      __riscv_vse8_v_i8mf8 (out + i + 300, v, vl);
      vint8mf8_t v2 = __riscv_vle8_v_i8mf8_tu (v, in + i + 200, vl);
      __riscv_vse8_v_i8mf8 (out + i + 200, v2, vl);
    }
}

/* { dg-final { scan-assembler-times {\.L[0-9]+\:\s+vle8\.v\s+v[0-9]+,\s*0\s*\([a-x0-9]+\)} 2 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
