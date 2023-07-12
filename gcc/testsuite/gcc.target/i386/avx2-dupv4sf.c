/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler-times "vbroadcastss" 2 } } */

typedef float __attribute__ ((vector_size (16))) v4sf;

v4sf bcst_reg (float f)
{
  register float x asm ("xmm7") = f;

  asm ("" : "+v" (x));
  return (v4sf) {x, x, x, x};
}

v4sf bcst_mem (const float *f)
{
  return (v4sf) {*f, *f, *f, *f};
}
