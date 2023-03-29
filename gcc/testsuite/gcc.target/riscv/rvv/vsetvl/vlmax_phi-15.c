/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32" } */

#include "riscv_vector.h"

/* The for loop body should not have vsetvl instruction.  */
void f (void * restrict in, void * restrict out, int n, int cond)
{
  switch (cond)
  {
  case 1:{
    vint8mf8_t v = *(vint8mf8_t*)(in + 100);
    *(vint8mf8_t*)(out + 100) = v;
    break;
  }
  case 2:{
    vint8mf8_t v = *(vint8mf8_t*)(in + 200);
    *(vint8mf8_t*)(out + 100) = v;
    break;
  }
  case 3:{
    vint8mf8_t v = *(vint8mf8_t*)(in + 300);
    *(vint8mf8_t*)(out + 100) = v;
    break;
  }
  default:{
    vint8mf8_t v = *(vint8mf8_t*)(in + 400);
    *(vint8mf8_t*)(out + 400) = v;
    break;
  }
  }
  for (int i = 0; i < n; i++)
    {
      vint8mf8_t v = *(vint8mf8_t*)(in + i);
      *(vint8mf8_t*)(out + i) = v;
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 4 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" no-opts "-flto" } } } } */
/* { dg-final { scan-assembler-times {ble\t[a-x0-9]+,zero,.L[0-9]+\s*\.L[0-9]+\:\s*vle8\.v\s+v[0-9]+,0\([a-x0-9]+\)} 1 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" no-opts "-flto"  } } } } */
