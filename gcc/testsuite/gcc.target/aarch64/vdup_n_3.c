/* { dg-do compile } */
/* { dg-options "-O2" } */

#define vector __attribute__((vector_size(4*sizeof(float))))

/* These are both dups. */
vector float f(vector float a, vector float b)
{
  return __builtin_shuffle (a, a, (vector int){0, 1, 0, 1});
}
vector float f1(vector float a, vector float b)
{
  return __builtin_shuffle (a, a, (vector int){2, 3, 2, 3});
}

/* { dg-final { scan-assembler-times {[ \t]*dup[ \t]+v[0-9]+\.2d} 2 } } */
