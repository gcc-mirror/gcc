/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f (int32_t * restrict in, int32_t * restrict out, int n, int cond)
{
  for (int i = 0; i < n; i++) {
    vint8mf8_t v = *(vint8mf8_t*)in;
    *(vint8mf8_t*)(out + i + 200) = v;
  }
  for (int i = 0; i < n; i++) {
    vint32mf2_t v = *(vint32mf2_t*)(in + 200);
    *(vint32mf2_t*)(out + i + 400) = v;
  }
  for (int i = 0; i < n; i++) {
    vint64m1_t v = *(vint64m1_t*)(in + 300);
    *(vint64m1_t*)(out + i + 400) = v;
  }
  for (int i = 0; i < n; i++) {
    vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 400);
    *(vfloat32mf2_t*)(out + i + 500) = v;
  }
  for (int i = 0; i < n; i++) {
    vfloat64m1_t v = *(vfloat64m1_t*)(in + 500);
    *(vfloat64m1_t*)(out + i + 600) = v;
  }

  switch (cond)
  {
  case 0:
  {
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] + 2;
      }
  }
  break;
  case 1:
  {
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] + 3;
      }
  }
  break;
  case 2:
  {
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] + 5;
      }
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] + 6;
      }
  }
  break;
  case 4:
  {
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] + 8;
      }
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] + 9;
      }
  }
  break;
  case 5:
  {
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] + 10;
      }
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] + 11;
      }
  }
  break;
  case 6:
  {
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] + 12;
      }
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] + 13;
      }
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] * out[i];
      }
  }
  break;
  case 7:
  {
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] + 15;
      }
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] + 16;
      }
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] + out[i];
      }
  }
  break;
  case 8:
  {
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] + 101;
      }
  }
  break;
  case 9:
  {
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] * 101;
      }
  }
  break;
  case 10:
  {
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] >> 3;
      }
  }
  break;
  default:
  {
      for (int i = 0; i < n; i++)
      {
          out[i] = out[i] << 1;
      }
  }
  break;
  }
  for (int i = 0; i < n; i++) {
    vint16mf4_t v;
    *(vint16mf4_t*)(out + i + 700) = v;
  }
}
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
