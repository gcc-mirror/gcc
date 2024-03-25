/* { dg-additional-options "-std=gnu89" } */

test_opt (a, b)
     unsigned a, b;
{
  a = a / b;
  if (a == 0)
    a++;
  return a;
}
