/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ccp1 -fdump-tree-optimized -Wno-psabi" } */
/* This is the vector version of these optimizations. */
/* PR tree-optimization/19832 */

#define vector __attribute__((vector_size(sizeof(unsigned)*2)))

static inline vector int max_(vector int a, vector int b)
{
   return (a > b)? a : b;
}
static inline vector int min_(vector int a, vector int b)
{
  return (a < b) ? a : b;
}

vector int f_minus(vector int a, vector int b)
{
  return (a != b) ? a - b : (a - a);
}
vector int f_xor(vector int a, vector int b)
{
  return (a != b) ? a ^ b : (a ^ a);
}

vector int f_ior(vector int a, vector int b)
{
  return (a != b) ? a | b : (a | a);
}
vector int f_and(vector int a, vector int b)
{
  return (a != b) ? a & b : (a & a);
}
vector int f_max(vector int a, vector int b)
{
  return (a != b) ? max_(a, b) : max_(a, a);
}
vector int f_min(vector int a, vector int b)
{
  return (a != b) ? min_(a, b) : min_(a, a);
}
vector int f_mult(vector int a, vector int b)
{
  return (a != b) ? a * b : (a * a);
}
vector int f_plus(vector int a, vector int b)
{
  return (a != b) ? a + b : (a + a);
}
vector int f_plus_alt(vector int a, vector int b)
{
  return (a != b) ? a + b : (a * 2);
}

/* All of the above function's VEC_COND_EXPR should have been optimized away. */
/* { dg-final { scan-tree-dump-not "VEC_COND_EXPR " "ccp1" } } */
/* { dg-final { scan-tree-dump-not "VEC_COND_EXPR " "optimized" } } */
