/* { dg-additional-options "-std=gnu89" } */

foo (a, b, p)
     short *p;
{
  p[0] = a;
  p[1] = b;
}
