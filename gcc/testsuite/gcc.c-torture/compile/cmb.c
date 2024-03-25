/* { dg-additional-options "-std=gnu89" } */

foo (p1, p2)
     short p1, *p2;
{
  int a;
  return (int) p1 + (int) *p2;
}
