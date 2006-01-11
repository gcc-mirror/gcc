/* { dg-do compile } */
/* { dg-options "-std=c99" } */

/* Decimal float keywords are not reserved for c99.  */

int _Decimal32 (void)
{
  return 0;
}

int foo (int i)
{
  int _Decimal64 = i * 2;
  return _Decimal64;
}
