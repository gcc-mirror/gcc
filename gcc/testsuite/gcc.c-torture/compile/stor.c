/* { dg-additional-options "-std=gnu89" } */

#define C 1

foo (p)
     int *p;
{
  p[0] = C;
  p[1] = C;
  p[2] = C;
  p[3] = C;
  p[4] = C;
  p[5] = C;
}
