/* { dg-do compile } */

void foo(double *d, double *a)
{
  d[0] += d[2];
  d[1] += d[3];
  d[2] += d[4];
  d[3] += d[5];
  a[0] = d[0];
  a[1] = d[1];
}
