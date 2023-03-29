/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vuint8mf8_t v = *(vuint8mf8_t*)(in + 100);
      *(vuint8mf8_t*)(out + 100) = v;
    }
  else
    {
      if (cond == 2)
        {
          vuint8mf8_t v = *(vuint8mf8_t*)(in + 200);
          *(vuint8mf8_t*)(out + 200) = v;
          out[1000] = 8000;
        }
    }
  for (int i = 0; i < n; i++)
    {
      vuint8mf8_t v = *(vuint8mf8_t*)(in + i);
      *(vuint8mf8_t*)(out + i) = v;
    }
}

void f2 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vuint8mf4_t v = *(vuint8mf4_t*)(in + 100);
      *(vuint8mf4_t*)(out + 100) = v;
    }
  else
    {
      if (cond == 2)
        {
          vuint8mf4_t v = *(vuint8mf4_t*)(in + 200);
          *(vuint8mf4_t*)(out + 200) = v;
          out[1000] = 8000;
        }
    }
  for (int i = 0; i < n; i++)
    {
      vuint8mf4_t v = *(vuint8mf4_t*)(in + i);
      *(vuint8mf4_t*)(out + i) = v;
    }
}

void f3 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vuint8mf2_t v = *(vuint8mf2_t*)(in + 100);
      *(vuint8mf2_t*)(out + 100) = v;
    }
  else
    {
      if (cond == 2)
        {
          vuint8mf2_t v = *(vuint8mf2_t*)(in + 200);
          *(vuint8mf2_t*)(out + 200) = v;
          out[1000] = 8000;
        }
    }
  for (int i = 0; i < n; i++)
    {
      vuint8mf2_t v = *(vuint8mf2_t*)(in + i);
      *(vuint8mf2_t*)(out + i) = v;
    }
}

void f4 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vuint16mf4_t v = *(vuint16mf4_t*)(in + 100);
      *(vuint16mf4_t*)(out + 100) = v;
    }
  else
    {
      if (cond == 2)
        {
          vuint16mf4_t v = *(vuint16mf4_t*)(in + 200);
          *(vuint16mf4_t*)(out + 200) = v;
          out[1000] = 8000;
        }
    }
  for (int i = 0; i < n; i++)
    {
      vuint16mf4_t v = *(vuint16mf4_t*)(in + i);
      *(vuint16mf4_t*)(out + i) = v;
    }
}

void f5 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vuint16mf2_t v = *(vuint16mf2_t*)(in + 100);
      *(vuint16mf2_t*)(out + 100) = v;
    }
  else
    {
      if (cond == 2)
        {
          vuint16mf2_t v = *(vuint16mf2_t*)(in + 200);
          *(vuint16mf2_t*)(out + 200) = v;
          out[1000] = 8000;
        }
    }
  for (int i = 0; i < n; i++)
    {
      vuint16mf2_t v = *(vuint16mf2_t*)(in + i);
      *(vuint16mf2_t*)(out + i) = v;
    }
}

void f6 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vuint32mf2_t v = *(vuint32mf2_t*)(in + 100);
      *(vuint32mf2_t*)(out + 100) = v;
    }
  else
    {
      if (cond == 2)
        {
          vuint32mf2_t v = *(vuint32mf2_t*)(in + 200);
          *(vuint32mf2_t*)(out + 200) = v;
          out[1000] = 8000;
        }
    }
  for (int i = 0; i < n; i++)
    {
      vuint32mf2_t v = *(vuint32mf2_t*)(in + i);
      *(vuint32mf2_t*)(out + i) = v;
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 3 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*t[au],\s*m[au]} 3 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 3 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 3 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*t[au],\s*m[au]} 3 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 3 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+\.L[0-9]\:} 1 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+\.L[0-9][0-9]\:} 1 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+\.L[0-9][0-9]\:} 1 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+\.L[0-9][0-9]\:} 1 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+\.L[0-9][0-9]\:} 1 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+\.L[0-9][0-9]\:} 1 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
