/* { dg-additional-options "-std=gnu89" } */

foo (a, b)
     int a, b;
{
  return (a < 0) | (a <= 0) | (a == 0) | (a != 0) | (a >= 0) | (a > 0);
}
