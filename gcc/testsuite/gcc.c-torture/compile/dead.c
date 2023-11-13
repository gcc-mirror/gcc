/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  ++a;
  return a == 0;
}
