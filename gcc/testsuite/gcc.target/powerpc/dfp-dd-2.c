/* Test generation of DFP instructions for POWER6.  */
/* { dg-do compile { target { powerpc*-*-linux* && powerpc_fprs } } } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-std=gnu99 -O1 -mdejagnu-cpu=power6" } */

/* { dg-final { scan-assembler-times "fneg" 1 } } */
/* { dg-final { scan-assembler-times "fabs" 1 } } */
/* { dg-final { scan-assembler-times "fnabs" 1 } } */
/* { dg-final { scan-assembler-times "fmr" 0 } } */

_Decimal64
func1 (_Decimal64 a, _Decimal64 b)
{
  return -b;
}

_Decimal64
func2 (_Decimal64 a, _Decimal64 b)
{
  return __builtin_fabsd64 (b);
}

_Decimal64
func3 (_Decimal64 a, _Decimal64 b)
{
  return - __builtin_fabsd64 (b);
}
