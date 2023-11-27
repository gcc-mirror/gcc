/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef float __attribute__((__vector_size__ (16))) F;

F foo (F a, F b)
{
  F v = (F) { 9 };
  return __builtin_shufflevector (v, v, 1, 0, 1, 2);
}

/* { dg-final { scan-tree-dump-not "VEC_PERM_EXPR" "optimized" } } */
/* { dg-final { scan-tree-dump "return \{ 0.0, 9.0e\\+0, 0.0, 0.0 \}" "optimized" } } */
