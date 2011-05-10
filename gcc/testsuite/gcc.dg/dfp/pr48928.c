/* PR debug/48928 */
/* { dg-do compile } */
/* { dg-options "-g -O2" } */

_Decimal32
foo (_Decimal32 x)
{
  _Decimal32 y = (x + x) / (9.DF * x);
  return y;
}
