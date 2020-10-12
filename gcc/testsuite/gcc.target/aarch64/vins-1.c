/* { dg-do compile } */
/* { dg-options "-O2" } */

#define vector __attribute__((vector_size(4*sizeof(float))))

vector float f0(vector float a, vector float b)
{
  return __builtin_shuffle (a, a, (vector int){3, 1, 2, 3});
}
vector float f1(vector float a, vector float b)
{
  return __builtin_shuffle (a, a, (vector int){0, 0, 2, 3});
}
vector float f2(vector float a, vector float b)
{
  return __builtin_shuffle (a, a, (vector int){0, 1, 0, 3});
}
vector float f3(vector float a, vector float b)
{
  return __builtin_shuffle (a, a, (vector int){0, 1, 2, 0});
}

/* { dg-final { scan-assembler-times {[ \t]*ins[ \t]+v[0-9]+\.s} 4 } } */
