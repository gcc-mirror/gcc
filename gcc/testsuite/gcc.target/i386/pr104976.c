/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -O2" } */

void
foo (int a, float b, double* c)
{
  union {
    _Float16 f[2];
    int i;} x;
  x.i = a;
  c[0] = x.f[0] * 0.2;
  c[1] = x.f[1] * 0.2;
}
