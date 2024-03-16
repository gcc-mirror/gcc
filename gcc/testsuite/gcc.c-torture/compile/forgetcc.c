/* { dg-additional-options "-std=gnu89" } */

foo (hp, p, a)
     short *hp;
     int *p;
     int a;
{
  hp[10] = a;
  p[0] = 10;
  if (hp[10] > 0)
    return 1;
  return 0;
}
