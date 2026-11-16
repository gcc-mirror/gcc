/* { dg-do compile } */
/* { dg-options "-O2 -ffinite-math-only -march=x86-64-v3 -mavx10.2 -mfpmath=sse" } */
/* { dg-final { scan-assembler-not "comx" } } */
/* { dg-final { scan-assembler-times "vcmp\[a-z_\]*s\[sd\]" 4 } } */
/* { dg-final { scan-assembler-times "vblendvp\[sd\]" 4 } } */
/* { dg-final { scan-assembler-not "cmov" } } */

float
sel_eq_ss (float a, float b, float c, float d)
{
  return a == b ? c : d;
}

float
sel_ne_ss (float a, float b, float c, float d)
{
  return a != b ? c : d;
}

double
sel_eq_sd (double a, double b, double c, double d)
{
  return a == b ? c : d;
}

double
sel_ne_sd (double a, double b, double c, double d)
{
  return a != b ? c : d;
}

_Float16
sel_eq_sh (_Float16 a, _Float16 b, _Float16 c, _Float16 d)
{
  return a == b ? c : d;
}

_Float16
sel_ne_sh (_Float16 a, _Float16 b, _Float16 c, _Float16 d)
{
  return a != b ? c : d;
}
