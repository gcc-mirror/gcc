/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do compile { target { lp64 } } } */
/* { dg-require-effective-target dfp } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-march=z13 -std=gnu99 -mno-hard-dfp" } */

#pragma GCC target("hard-dfp")
_Decimal64 p1(_Decimal64 f, _Decimal64 g)
{
  return f * g;
  /* { dg-final { scan-assembler-times "\tmdtr\t" 1 } } */
}
#pragma GCC reset_options

#pragma GCC target("no-hard-dfp")
	      _Decimal64 p0(_Decimal64 f, _Decimal64 g)
{
  return f / 2;
  /* { dg-final { scan-assembler-not "\tddtr\t" } } */
}
#pragma GCC reset_options

__attribute__ ((target("hard-dfp")))
_Decimal64 a1(_Decimal64 f, _Decimal64 g)
{
  /* { dg-final { scan-assembler-times "\tadtr\t" 1 } } */
  return f + g;
}

__attribute__ ((target("no-hard-dfp")))
_Decimal64 a0(_Decimal64 f, _Decimal64 g)
{
  return f - g;
  /* { dg-final { scan-assembler-not "\tsdtr\t" } } */
}

_Decimal64 d(_Decimal64 f, _Decimal64 g)
{
  return f - g;
  /* { dg-final { scan-assembler-not "\tsdtr\t" } } */
}
