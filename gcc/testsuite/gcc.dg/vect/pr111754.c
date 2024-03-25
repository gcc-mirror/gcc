/* PR middle-end/111754 */
/* { dg-do compile } */
/* { dg-additional-options "-O2 -fdump-tree-forwprop1 -Wno-psabi" } */

typedef float __attribute__((__vector_size__ (16))) F;

F foo (F a, F b)
{
  F v = (F) { 9 };
  return __builtin_shufflevector (v, v, 1, 0, 1, 2);
}

/* { dg-final { scan-tree-dump-not "VEC_PERM_EXPR" "forwprop1" } } */
/* { dg-final { scan-tree-dump "(return|<retval> =) \{ 0.0, 9.0e\\+0, 0.0, 0.0 \}" "forwprop1" } } */
