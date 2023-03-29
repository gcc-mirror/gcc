/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32" } */

#include "riscv_vector.h"

/* The for loop body should not have vsetvl instruction.  */
void f (void * restrict in, void * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vint8mf2_t v = *(vint8mf2_t*)(in + 100);
      *(vint8mf2_t*)(out + 100) = v;
    }
  else if (cond == 2)
    {
      vint8mf2_t v = *(vint8mf2_t*)(in + 200);
      *(vint8mf2_t*)(out + 200) = v;
    }
  else if (cond == 3)
    {
      vint8mf2_t v = *(vint8mf2_t*)(in + 300);
      *(vint8mf2_t*)(out + 300) = v;
    }
  for (int i = 0; i < n; i++)
    {
      vint8mf2_t v = *(vint8mf2_t*)(in + i);
      *(vint8mf2_t*)(out + i) = v;
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 4 { target { no-opts "-O0"   no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */

