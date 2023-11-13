/* { dg-additional-options "-std=gnu89" } */

foo (p, a, b)
     int *p;
     int a;
     int b;
{

  a += p[0];
  b += p[1];
  if (a == 0)
    return b;
  return a;
}


bar (a)
{
  return -a > 0 ? 1 : 2;
}
