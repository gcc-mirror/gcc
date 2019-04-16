/* Test generation of DFP instructions for POWER6.  */
/* { dg-do compile { target { powerpc*-*-linux* && powerpc_fprs } } } */
/* { dg-options "-std=gnu99 -O1 -mdejagnu-cpu=power6" } */

/* { dg-final { scan-assembler-times "fneg" 1 } } */
/* { dg-final { scan-assembler-times "fabs" 1 } } */
/* { dg-final { scan-assembler-times "fnabs" 1 } } */
/* { dg-final { scan-assembler-times "fmr" 0 } } */

/* These tests verify we only generate fneg, fabs and fnabs
   instructions and no fmr's since these are done in place.  */

_Decimal128
func1 (_Decimal128 a)
{
  return -a;
}

_Decimal128
func2 (_Decimal128 a)
{
  return __builtin_fabsd128 (a);
}

_Decimal128
func3 (_Decimal128 a)
{
  return - __builtin_fabsd128 (a);
}
