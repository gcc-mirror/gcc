/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvfhmin -mabi=ilp32d -mrvv-vector-bits=scalable -ffast-math -fdump-rtl-final" } */

#include <stdint-gcc.h>

void f0 (_Float16 * __restrict a, _Float16 * __restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = -b[i];
}

void f1 (_Float16 * __restrict a, _Float16 * __restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = a[i]+b[i];
}

void f2 (_Float16 * __restrict a, _Float16 * __restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = a[i]-b[i];
}

void f3 (_Float16 * __restrict a, _Float16 * __restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = a[i]*b[i];
}

void f4 (_Float16 * __restrict a, _Float16 * __restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = a[i]/b[i];
}

void f6 (_Float16 * __restrict a, _Float16 * __restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = __builtin_fabs (b[i]);
}

void f7 (_Float16 * __restrict a, _Float16 * __restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = __builtin_sqrtf16 (b[i]);
}

void f8 (_Float16 * __restrict a, int16_t * __restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = (_Float16) (b[i]);
}

void f9 (_Float16 * __restrict a, uint16_t * __restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = (_Float16) (b[i]);
}

void f10 (int16_t * __restrict a, _Float16 * __restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = (int16_t) (b[i]);
}

void f11 (uint16_t * __restrict a, _Float16 * __restrict b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = (uint16_t) (b[i]);
}

/* We can't enable FP16 NEG/PLUS/MINUS/MULT/DIV/ABS/SQRTF as well as int/float
   conversion auto-vectorization when -march="*zvfhmin*" because the min
   variant of the extension only provides loads, stores and conversions.
   As we might still vectorize after promotion to float, we need to make
   sure that no vector operations with an HFmode are being generated.  */
/* { dg-final { scan-rtl-dump-not "plus:VNx\[0-9\]+HF" "final" } } */
/* { dg-final { scan-rtl-dump-not "minus:VNx\[0-9\]+HF" "final" } } */
/* { dg-final { scan-rtl-dump-not "mult:VNx\[0-9\]+HF" "final" } } */
/* { dg-final { scan-rtl-dump-not "div:VNx\[0-9\]+HF" "final" } } */
/* { dg-final { scan-rtl-dump-not "neg:VNx\[0-9\]+HF" "final" } } */
/* { dg-final { scan-rtl-dump-not "abs:VNx\[0-9\]+HF" "final" } } */
/* { dg-final { scan-rtl-dump-not "sqrt:VNx\[0-9\]+HF" "final" } } */
/* { dg-final { scan-rtl-dump-not "float:VNx\[0-9\]+HF" "final" } } */
/* { dg-final { scan-rtl-dump-not "fix:VNx\[0-9\]+HI\s*.+reg:VNx\[0-9\]+HF" "final" } } */
