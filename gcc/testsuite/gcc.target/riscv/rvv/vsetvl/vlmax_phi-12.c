/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32" } */

#include "riscv_vector.h"

/* The for loop body should not have vsetvl instruction.  */
void f (void * restrict in, void * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vuint32mf2_t v = *(vuint32mf2_t*)(in + 100);
      *(vuint32mf2_t*)(out + 100) = v;
    }
  else if (cond == 2)
    {
      vuint32mf2_t v = *(vuint32mf2_t*)(in + 200);
      *(vuint32mf2_t*)(out + 200) = v;
    }
  else if (cond == 3)
    {
      vuint32mf2_t v = *(vuint32mf2_t*)(in + 300);
      *(vuint32mf2_t*)(out + 300) = v;
    }
  else
    {
      vuint32mf2_t v = *(vuint32mf2_t*)(in + 400);
      *(vuint32mf2_t*)(out + 400) = v;
    }
  for (int i = 0; i < n; i++)
    {
      vuint32mf2_t v = *(vuint32mf2_t*)(in + i);
      *(vuint32mf2_t*)(out + i) = v;
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 4 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" no-opts "-flto" } } } } */
/* { dg-final { scan-assembler-times {ble\t[a-x0-9]+,zero,.L[0-9]+\s*\.L[0-9]+\:\s*vle32\.v\s+v[0-9]+,0\([a-x0-9]+\)} 1 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" no-opts "-flto" } } } } */
