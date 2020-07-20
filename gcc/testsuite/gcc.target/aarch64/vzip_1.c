/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target aarch64_little_endian } */

#define vector __attribute__((vector_size(2*sizeof(float))))

vector float f(vector float a, vector float b)
{
  return __builtin_shuffle (a, b, (vector int){0, 2});
}

/* { dg-final { scan-assembler-times {[ \t]*zip1[ \t]+v[0-9]+\.2s} 1 } } */
