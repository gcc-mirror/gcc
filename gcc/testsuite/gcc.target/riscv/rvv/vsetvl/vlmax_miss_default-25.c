/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32" } */

#include "riscv_vector.h"

/* The for loop body should not have vsetvl instruction.  */
void f (void * restrict in, void * restrict out, int n, int cond)
{
  switch (cond)
  {
  case 1:{
    vint32mf2_t v = *(vint32mf2_t*)(in + 100);
    *(vint32mf2_t*)(out + 100) = v;
    break;
  }
  case 2:{
    vint32mf2_t v = *(vint32mf2_t*)(in + 200);
    *(vint32mf2_t*)(out + 100) = v;
    break;
  }
  case 3:{
    vint32mf2_t v = *(vint32mf2_t*)(in + 300);
    *(vint32mf2_t*)(out + 100) = v;
    break;
  }
  default:{
    break;
  }
  }
  for (int i = 0; i < n; i++)
    {
      vint32mf2_t v = *(vint32mf2_t*)(in + i);
      *(vint32mf2_t*)(out + i) = v;
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 4 { target { no-opts "-O0"   no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+\.L[0-9]\:} 1 { target { no-opts "-O0"   no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
