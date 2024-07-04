/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  if (a & 38)
    return 1;
  return 0;
}
