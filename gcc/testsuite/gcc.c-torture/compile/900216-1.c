/* { dg-additional-options "-std=gnu89" } */

foo (p, a, b)
     unsigned short *p;
{
  unsigned int x;

  x = p[0];

  return (x == 134U);
}
