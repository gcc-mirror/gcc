/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target aarch64_little_endian } */

#define vector __attribute__((vector_size(4*sizeof(float))))

vector float f(vector float a, vector float b)
{
  /* This is the same as zip2 v.2d as {2, 3, 6, 7} can be converted to {1, 3}. */
  return __builtin_shuffle (a, b, (vector int){2, 3, 6, 7});
}

/* { dg-final { scan-assembler-times {[ \t]*zip2[ \t]+v[0-9]+\.2d} 1 } } */
