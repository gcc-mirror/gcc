/* { dg-additional-options "-std=gnu89" } */

foo (a)
     double a;
{
  if (a >= 0)
    return 1;
  return a;
}
