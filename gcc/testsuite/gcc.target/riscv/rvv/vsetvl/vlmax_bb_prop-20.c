/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vbool64_t v = *(vbool64_t*)(in + 100);
      *(vbool64_t*)(out + 100) = v;
    }
  else
    {
      vbool64_t v = *(vbool64_t*)(in + 200);
      *(vbool64_t*)(out + 200) = v;
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
      vbool64_t v = *(vbool64_t*)(in + i);
      *(vbool64_t*)(out + i) = v;
    }
}

void f2 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vbool32_t v = *(vbool32_t*)(in + 100);
      *(vbool32_t*)(out + 100) = v;
    }
  else
    {
      vbool32_t v = *(vbool32_t*)(in + 200);
      *(vbool32_t*)(out + 200) = v;
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
      vbool32_t v = *(vbool32_t*)(in + i);
      *(vbool32_t*)(out + i) = v;
    }
}

void f3 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vbool16_t v = *(vbool16_t*)(in + 100);
      *(vbool16_t*)(out + 100) = v;
    }
  else
    {
      vbool16_t v = *(vbool16_t*)(in + 200);
      *(vbool16_t*)(out + 200) = v;
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
      vbool16_t v = *(vbool16_t*)(in + i);
      *(vbool16_t*)(out + i) = v;
    }
}

void f4 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vbool8_t v = *(vbool8_t*)(in + 100);
      *(vbool8_t*)(out + 100) = v;
    }
  else
    {
      vbool8_t v = *(vbool8_t*)(in + 200);
      *(vbool8_t*)(out + 200) = v;
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
      vbool8_t v = *(vbool8_t*)(in + i);
      *(vbool8_t*)(out + i) = v;
    }
}

void f5 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vbool4_t v = *(vbool4_t*)(in + 100);
      *(vbool4_t*)(out + 100) = v;
    }
  else
    {
      vbool4_t v = *(vbool4_t*)(in + 200);
      *(vbool4_t*)(out + 200) = v;
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
      vbool4_t v = *(vbool4_t*)(in + i);
      *(vbool4_t*)(out + i) = v;
    }
}

void f6 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vbool2_t v = *(vbool2_t*)(in + 100);
      *(vbool2_t*)(out + 100) = v;
    }
  else
    {
      vbool2_t v = *(vbool2_t*)(in + 200);
      *(vbool2_t*)(out + 200) = v;
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
      vbool2_t v = *(vbool2_t*)(in + i);
      *(vbool2_t*)(out + i) = v;
    }
}

void f7 (int * restrict in, int * restrict out, int n, int cond)
{
  if (cond == 1)
    {
      vbool1_t v = *(vbool1_t*)(in + 100);
      *(vbool1_t*)(out + 100) = v;
    }
  else
    {
      vbool1_t v = *(vbool1_t*)(in + 200);
      *(vbool1_t*)(out + 200) = v;
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
      vbool1_t v = *(vbool1_t*)(in + i);
      *(vbool1_t*)(out + i) = v;
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m1,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m4,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {add\ta[0-7],a[0-7],a[0-7]\s+\.L[0-9]\:\s+vlm\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)} 1 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {add\ta[0-7],a[0-7],a[0-7]\s+\.L[0-9][0-9]\:\s+vlm\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)} 6 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
