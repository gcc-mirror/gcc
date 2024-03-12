/* PR target/110832 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -ftrapping-math -msse2 -mpartial-vector-fp-math -dp" } */

typedef float __attribute__((vector_size(8))) v2sf;

v2sf test (v2sf a, v2sf b)
{
  return a + b;
}

/* { dg-final { scan-assembler "addps" } } */
/* { dg-final { scan-assembler-times "\\*vec_concatv4sf_0" 2 } } */
