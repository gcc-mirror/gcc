/* { dg-additional-options "-std=gnu89" } */

foo (a)
{
  if (a >= 0)
    return (unsigned) a << 10;
  else
    return (int) a << 10;
}
