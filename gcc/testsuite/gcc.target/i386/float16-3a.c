/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */

_Float16
foo (int x)
{
  return x;
}

/* { dg-final { scan-assembler-times "vcvtsi2shl\[ \t\]+\[^\n\r]*%xmm0" 1 } } */
