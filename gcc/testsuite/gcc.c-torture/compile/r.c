/* { dg-additional-options "-std=gnu89" } */

r (a, b)
{
  if (a < b)
    return 1;
  else
    return 2;
}
