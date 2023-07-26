/* PR target/110762 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -dp" } */

typedef float v2sf __attribute__((vector_size(8)));
typedef float v4sf __attribute__((vector_size(16)));

v2sf test(v4sf x, v4sf y)
{
  v2sf x2, y2;

  x2 = __builtin_shufflevector (x, x, 0, 1);
  y2 = __builtin_shufflevector (y, y, 0, 1);

  return x2 + y2;
}

/* { dg-final { scan-assembler-times "\\*vec_concatv4sf_0" 2 } } */
