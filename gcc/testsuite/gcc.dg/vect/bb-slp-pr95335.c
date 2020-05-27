/* { dg-do compile } */

float *a;
float b;
void
fn1(float p1[][3])
{
  float c, d, e, f;
  f = a[1] * a[1] * d;
  b = a[1] * a[2] * d;
  p1[1][1] = f + c;
  p1[1][2] = b + e;
}
