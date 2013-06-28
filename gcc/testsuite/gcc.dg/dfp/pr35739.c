/* PR c/35739 */
/* { dg-do compile { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-O -fpreprocessed -fmudflap" } */

_Decimal128
foo (int n, ...)
{
  int i;
  _Decimal128 j = 0;
  __builtin_va_list ap;
  __builtin_va_start (ap, n);
  for (i = 0; i < n; i++)
    j += __builtin_va_arg (ap, _Decimal128);
  __builtin_va_end (ap);
  return j;
}
