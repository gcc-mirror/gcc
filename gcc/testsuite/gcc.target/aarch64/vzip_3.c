/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target aarch64_little_endian } */

#define vector __attribute__((vector_size(4*sizeof(float))))

vector float f(vector float a, vector float b)
{
  /* This is the same as zip1 v.2d as {4, 5, 0, 1} can be converted to {2, 0}. */
  return __builtin_shuffle (a, b, (vector int){4, 5, 0, 1});
}

/* { dg-final { scan-assembler-times {[ \t]*zip1[ \t]+v[0-9]+\.2d} 1 } } */
