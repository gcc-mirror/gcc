/* { dg-additional-options "-std=gnu89" } */

foo (a, b, c)
     double a;
     int b;
     double c;
{
  if (b)
    return a + c;
  else
    return a - c;
}
