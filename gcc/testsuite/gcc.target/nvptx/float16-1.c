/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -misa=sm_53 -mptx=_" } */
/* { dg-additional-options "-mexperimental" } */

_Float16 var;

float load()
{
  return var;
}

void store(float x)
{
  var = x;
}

void move(_Float16 *dst, _Float16 *src)
{
  *dst = *src;
}

double plus(double x, double y)
{
  _Float16 hx = x;
  _Float16 hy = y;
  _Float16 hz = hx + hy;
  return hz;
}

double minus(double x, double y)
{
  _Float16 hx = x;
  _Float16 hy = y;
  _Float16 hz = hx - hy;
  return hz;
}

double mult(double x, double y)
{
  _Float16 hx = x;
  _Float16 hy = y;
  _Float16 hz = hx * hy;
  return hz;
}

/* { dg-final { scan-assembler-times "ld.b16" 2 } } */
/* { dg-final { scan-assembler-times "cvt.f32.f16" 1 } } */
/* { dg-final { scan-assembler-times "cvt.rn.f16.f32" 1 } } */
/* { dg-final { scan-assembler-times "st.b16" 2 } } */
/* { dg-final { scan-assembler-times "add.f16" 1 } } */
/* { dg-final { scan-assembler-times "sub.f16" 1 } } */
/* { dg-final { scan-assembler-times "mul.f16" 1 } } */
/* { dg-final { scan-assembler-times "cvt.rn.f16.f64" 6 } } */
/* { dg-final { scan-assembler-times "cvt.f64.f16" 3 } } */
