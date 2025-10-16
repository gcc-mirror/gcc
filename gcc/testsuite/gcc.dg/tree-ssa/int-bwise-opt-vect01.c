/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1 -fdump-tree-optimized" } */

/* PR tree-optimization/122296 */

typedef unsigned type1 __attribute__((vector_size(sizeof(unsigned))));

type1 f(type1 a, type1 b)
{
  type1 c = a == b;
  type1 d = (a|b) != 0;
  return c | d;
}

/* { dg-final { scan-tree-dump-not "VEC_COND_EXPR " "optimized" } } */
/* { dg-final { scan-tree-dump-not "VEC_COND_EXPR " "forwprop1" } } */

