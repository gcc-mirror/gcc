/* i?86 does not have V2SF, x32 does though.  */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -mavx -mfma" } */

struct Matrix
{
  float m11;
  float m12;
  float m21;
  float m22;
  float dx;
  float dy;
};

struct Matrix multiply(const struct Matrix *a, const struct Matrix *b)
{
  struct Matrix out;
  out.m11 = a->m11*b->m11 + a->m12*b->m21;
  out.m12 = a->m11*b->m12 + a->m12*b->m22;
  out.m21 = a->m21*b->m11 + a->m22*b->m21;
  out.m22 = a->m21*b->m12 + a->m22*b->m22;

  out.dx = a->dx*b->m11  + a->dy*b->m21 + b->dx;
  out.dy = a->dx*b->m12  + a->dy*b->m22 + b->dy;
  return out;
}

/* The whole kernel should be vectorized with V4SF and V2SF operations.  */
/* { dg-final { scan-assembler-times "vadd" 1 } } */
/* { dg-final { scan-assembler-times "vmul" 2 } } */
/* { dg-final { scan-assembler-times "vfma" 2 } } */
