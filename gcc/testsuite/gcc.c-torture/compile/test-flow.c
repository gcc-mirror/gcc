/* { dg-additional-options "-std=gnu89" } */

foo (a, b, c, d)
{
  if (a < 0)
    {
      b = c;
    }
  else
    {
      b = d;
    }
  return b + 75;
}
