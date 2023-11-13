/* { dg-additional-options "-std=gnu89" } */

log2 (a, b)
{
  int c;
  c = ~(~a & ~b);
  return c;
}
