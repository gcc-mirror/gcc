/* PR middle-end/127882 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-ssse3 -fdump-tree-optimized" } */

typedef short v8hi __attribute__((vector_size(16)));
typedef _Float16 v8hf __attribute__((vector_size(16)));

/* Both permutes are supported, but the merged one is not: keep them, also
   with a VIEW_CONVERT_EXPR between them.  */

v8hi
f1 (v8hi a, v8hi b)
{
  v8hi c = __builtin_shufflevector (a, b, 8, 0, 9, 1, 10, 2, 11, 3);
  return __builtin_shufflevector (c, c, 1, 1, 0, 0, 4, 5, 7, 7);
}

v8hf
f2 (v8hi a, v8hi b)
{
  v8hi c = __builtin_shufflevector (a, b, 8, 0, 9, 1, 10, 2, 11, 3);
  v8hf e = (v8hf) c;
  return __builtin_shufflevector (e, e, 1, 1, 0, 0, 4, 5, 7, 7);
}

/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-not "BIT_FIELD_REF" "optimized" } } */
