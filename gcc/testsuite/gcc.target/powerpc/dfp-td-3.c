/* Test generation of DFP instructions for POWER6.  */
/* { dg-do compile { target { powerpc*-*-linux* && powerpc_fprs } } } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-std=gnu99 -O1 -mdejagnu-cpu=power6" } */

/* { dg-final { scan-assembler-times "fneg" 1 } } */
/* { dg-final { scan-assembler-times "fabs" 1 } } */
/* { dg-final { scan-assembler-times "fnabs" 1 } } */
/* { dg-final { scan-assembler-times "fmr" 3 } } */

/* These tests verify we generate fneg, fabs and fnabs and
   associated fmr's since these are not done in place.  */

_Decimal128
func1 (_Decimal128 a, _Decimal128 b)
{
  return -b;
}

_Decimal128
func2 (_Decimal128 a, _Decimal128 b)
{
  return __builtin_fabsd128 (b);
}

_Decimal128
func3 (_Decimal128 a, _Decimal128 b)
{
  return - __builtin_fabsd128 (b);
}
