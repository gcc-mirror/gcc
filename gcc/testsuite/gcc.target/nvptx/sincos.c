/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

extern float sinf (float);
extern float cosf (float);

float
sincos_add (float x)
{
  float s = sinf (x);
  float c = cosf (x);

  return s + c;
}

/* { dg-final { scan-assembler-times "sin.approx.f32" 1 } } */
/* { dg-final { scan-assembler-times "cos.approx.f32" 1 } } */
