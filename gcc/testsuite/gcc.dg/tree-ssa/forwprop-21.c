/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-copyprop1" } */
typedef int v4si __attribute__ ((vector_size (4 * sizeof(int))));

int
test (v4si *x, v4si *y)
{
  v4si m = { 2, 3, 6, 5 };
  v4si z = __builtin_shuffle (*x, *y, m);
  return z[2];
}

/* Optimization in forwprop1, cleanup in copyprop1.  */

/* { dg-final { scan-tree-dump-not "VEC_PERM_EXPR" "copyprop1" } } */
/* { dg-final { cleanup-tree-dump "copyprop1" } } */
