/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

typedef int vec __attribute__((vector_size (4 * sizeof (int))));
void f (vec *x1, vec *x2)
{
  vec m = { 1, 2, 3, 0 };
  vec n = { 3, 0, 1, 2 };
  vec y = __builtin_shuffle (*x1, *x2, n);
  vec z = __builtin_shuffle (y, m);
  *x1 = z;
}

/* { dg-final { scan-tree-dump-not "VEC_PERM_EXPR" "forwprop1" } } */
