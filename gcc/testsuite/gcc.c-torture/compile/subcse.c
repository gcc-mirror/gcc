/* { dg-additional-options "-std=gnu89" } */

foo (a, b, p)
     int *p;
{
  p[0] = 1230 - a;
  p[1] = 1230 - b;
}
