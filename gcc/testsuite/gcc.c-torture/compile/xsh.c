/* { dg-additional-options "-std=gnu89" } */

foo (a, b)
{
  a = b + b;
  if (a)
    return a;
  return b;
}
