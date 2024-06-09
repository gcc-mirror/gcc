/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vint8mf8_t v = *(vint8mf8_t*)(in + 100);
      *(vint8mf8_t*)(out + 100) = v;
    }
  else
    {
      vint8mf8_t v = *(vint8mf8_t*)(in + 200);
      *(vint8mf8_t*)(out + 200) = v;
      if (cond == 2)
        {
          out[1000] = 8000;
        }
      else
        {
          out[2000] = 9000;
        }
    }
  for (int i = 0; i < n; i++)
    {
      vint8mf8_t v = *(vint8mf8_t*)(in + i);
      *(vint8mf8_t*)(out + i) = v;
    }
}

void f2 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vint8mf4_t v = *(vint8mf4_t*)(in + 100);
      *(vint8mf4_t*)(out + 100) = v;
    }
  else
    {
      vint8mf4_t v = *(vint8mf4_t*)(in + 200);
      *(vint8mf4_t*)(out + 200) = v;
      if (cond == 2)
        {
          out[1000] = 8000;
        }
      else
        {
          out[2000] = 9000;
        }
    }
  for (int i = 0; i < n; i++)
    {
      vint8mf4_t v = *(vint8mf4_t*)(in + i);
      *(vint8mf4_t*)(out + i) = v;
    }
}

void f3 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vint8mf2_t v = *(vint8mf2_t*)(in + 100);
      *(vint8mf2_t*)(out + 100) = v;
    }
  else
    {
      vint8mf2_t v = *(vint8mf2_t*)(in + 200);
      *(vint8mf2_t*)(out + 200) = v;
      if (cond == 2)
        {
          out[1000] = 8000;
        }
      else
        {
          out[2000] = 9000;
        }
    }
  for (int i = 0; i < n; i++)
    {
      vint8mf2_t v = *(vint8mf2_t*)(in + i);
      *(vint8mf2_t*)(out + i) = v;
    }
}

void f4 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vint16mf4_t v = *(vint16mf4_t*)(in + 100);
      *(vint16mf4_t*)(out + 100) = v;
    }
  else
    {
      vint16mf4_t v = *(vint16mf4_t*)(in + 200);
      *(vint16mf4_t*)(out + 200) = v;
      if (cond == 2)
        {
          out[1000] = 8000;
        }
      else
        {
          out[2000] = 9000;
        }
    }
  for (int i = 0; i < n; i++)
    {
      vint16mf4_t v = *(vint16mf4_t*)(in + i);
      *(vint16mf4_t*)(out + i) = v;
    }
}

void f5 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vint16mf2_t v = *(vint16mf2_t*)(in + 100);
      *(vint16mf2_t*)(out + 100) = v;
    }
  else
    {
      vint16mf2_t v = *(vint16mf2_t*)(in + 200);
      *(vint16mf2_t*)(out + 200) = v;
      if (cond == 2)
        {
          out[1000] = 8000;
        }
      else
        {
          out[2000] = 9000;
        }
    }
  for (int i = 0; i < n; i++)
    {
      vint16mf2_t v = *(vint16mf2_t*)(in + i);
      *(vint16mf2_t*)(out + i) = v;
    }
}

void f6 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vint32mf2_t v = *(vint32mf2_t*)(in + 100);
      *(vint32mf2_t*)(out + 100) = v;
    }
  else
    {
      vint32mf2_t v = *(vint32mf2_t*)(in + 200);
      *(vint32mf2_t*)(out + 200) = v;
      if (cond == 2)
        {
          out[1000] = 8000;
        }
      else
        {
          out[2000] = 9000;
        }
    }
  for (int i = 0; i < n; i++)
    {
      vint32mf2_t v = *(vint32mf2_t*)(in + i);
      *(vint32mf2_t*)(out + i) = v;
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {add\ta[0-7],a[0-7],a[0-7]\s+\.L[0-9]\:\s+vle8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {add\ta[0-7],a[0-7],a[0-7]\s+\.L[0-9][0-9]\:\s+vle8\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)} 2 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {add\ta[0-7],a[0-7],a[0-7]\s+\.L[0-9][0-9]\:\s+vle16\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)} 2 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {add\ta[0-7],a[0-7],a[0-7]\s+\.L[0-9][0-9]\:\s+vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
