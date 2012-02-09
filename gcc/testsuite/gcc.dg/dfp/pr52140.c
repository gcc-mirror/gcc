/* { dg-do compile } */
/* { dg-options "-O1" } */

/* This used to result in an ICE.  */

int
foo (_Decimal64 x, _Decimal64 y)
{
  return (x < y) || (x > y);
}
