/* { dg-do compile } */
/* { dg-options "-O2" } */

#define vector __attribute__((vector_size(8*sizeof(short))))

vector short f0(vector short a, vector short b)
{
  return __builtin_shuffle (a, a, (vector short){6, 1, 2, 3, 4, 5, 6, 7});
}
vector short f2(vector short a, vector short b)
{
  return __builtin_shuffle (a, a, (vector short){0, 1, 2, 1, 4, 5, 6, 7});
}
vector short f4(vector short a, vector short b)
{
  return __builtin_shuffle (a, a, (vector short){0, 1, 2, 3, 0, 5, 6, 7});
}
vector short f6(vector short a, vector short b)
{
  return __builtin_shuffle (a, a, (vector short){0, 1, 2, 3, 4, 5, 6, 1});
}

/* { dg-final { scan-assembler-times {[ \t]*ins[ \t]+v[0-9]+\.h} 4 } } */
