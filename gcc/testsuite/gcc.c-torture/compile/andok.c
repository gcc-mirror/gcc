/* { dg-additional-options "-std=gnu89" } */

foo (a, b, p)
     int *p;
{
  p[1] = a & 0xfff0000;
  p[2] = b & 0xfff0000;
}
