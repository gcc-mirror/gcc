/* { dg-additional-options "-std=gnu89" } */

foo (ip, a, x)
     int a;
     int *ip;
     int x;
{
  if (a >= 1)
    x++;
  return x;
}
