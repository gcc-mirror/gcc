/* { dg-additional-options "-std=gnu89" } */

int
foo (a, p)
     int *p;
{
  p[0] = 85 * a;
  p[1] = -86 * a;
}
