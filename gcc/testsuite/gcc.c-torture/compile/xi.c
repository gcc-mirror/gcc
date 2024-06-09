/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  int r = 0;
  if (a)
    r = 1;
  return r;
}
