/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512fp16" } */

_Float16
foo (unsigned long long x)
{
  return x;
}

/* { dg-final { scan-assembler-times "vcvtusi2shq\[ \t\]+\[^\n\r]*%xmm0" 1 } } */
