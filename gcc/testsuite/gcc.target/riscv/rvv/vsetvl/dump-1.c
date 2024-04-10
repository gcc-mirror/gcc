/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fdump-rtl-vsetvl-details" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, void * restrict in2, void * restrict out2,  int n, int cond)
{
  for (int i = 0; i < n; i++)
    {
      vuint16mf4_t v2;
      *(vuint16mf4_t*)(out + i + 1000) = v2;
      vbool32_t v4;
      *(vbool32_t*)(out + i + 3000) = v4;
      vbool16_t v5;
      *(vbool16_t*)(out + i + 4000) = v5;
      vbool8_t v6;
      *(vbool8_t*)(out + i + 5000) = v6;
      vbool4_t v7;
      *(vbool4_t*)(out + i + 6000) = v7;
      vbool2_t v8;
      *(vbool2_t*)(out + i + 7000) = v8;
      vbool1_t v9;
      *(vbool1_t*)(out + i + 8000) = v9;
      vuint32mf2_t v10;
      *(vuint32mf2_t*)(out + i + 100000) = v10;
    }
  
  for (int i = 0; i < n; i++) 
    {
      vint8mf8_t v1 = *(vint8mf8_t*)(in + i + 100000);
      *(vint8mf8_t*)(out + i + 10) = v1;
    }
}
