/* { dg-do compile } */
/* { dg-options "-O2" } */

#define vector __attribute__((vector_size(4*sizeof(float))))

vector float f0(vector float a, vector float b)
{
  return __builtin_shuffle (a, b, (vector int){0, 5, 6, 7});
}
vector float f1(vector float a, vector float b)
{
  return __builtin_shuffle (a, b, (vector int){4, 0, 6, 7});
}
vector float f2(vector float a, vector float b)
{
  return __builtin_shuffle (a, b, (vector int){4, 5, 0, 7});
}
vector float f3(vector float a, vector float b)
{
  return __builtin_shuffle (a, b, (vector int){4, 5, 6, 0});
}

/* { dg-final { scan-assembler-times {[ \t]*ins[ \t]+v[0-9]+\.s} 4 } } */
