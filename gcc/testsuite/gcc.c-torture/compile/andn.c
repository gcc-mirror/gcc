/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  int b = 0x1fff;
  return a & ~b;
}
