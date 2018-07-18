/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target hard_dfp } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "dxex "  1 } } */
/* { dg-final { scan-assembler-times "dxexq " 1 } } */
/* { dg-final { scan-assembler-times "diex "  1 } } */
/* { dg-final { scan-assembler-times "diexq " 1 } } */
/* { dg-final { scan-assembler-not "bl __builtin" } } */
/* Verify we don't generate any drintn., drintnq., dctfix, dctfixq, dcffix
   or dcffixq instructions, as they imply we are getting unwanted casting.  */
/* { dg-final { scan-assembler-not "drintn\[q\]\." } } */
/* { dg-final { scan-assembler-not "dctfix\[q\]" } } */
/* { dg-final { scan-assembler-not "dcffix\[q\]" } } */

long long
do_xex (_Decimal64 arg)
{
  return __builtin_dxex (arg);
}

long long
do_xexq (_Decimal128 arg)
{
  return __builtin_dxexq (arg);
}

_Decimal64
do_iex (long long exp, _Decimal64 arg)
{
  return __builtin_diex (exp, arg);
}

_Decimal128
do_iexq (long long exp, _Decimal128 arg)
{
  return __builtin_diexq (exp, arg);
}
