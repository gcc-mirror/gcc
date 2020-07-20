/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target aarch64_little_endian } */

#define vector __attribute__((vector_size(4*sizeof(float))))

vector float f(vector float a, vector float b)
{
  /* This is the same as zip1 v.2d as {0, 1, 4, 5} can be converted to {0, 2}. */
  return __builtin_shuffle (a, b, (vector int){0, 1, 4, 5});
}

/* { dg-final { scan-assembler-times {[ \t]*zip1[ \t]+v[0-9]+\.2d} 1 } } */
