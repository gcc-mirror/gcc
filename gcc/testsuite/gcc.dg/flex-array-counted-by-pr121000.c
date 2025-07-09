/* PR middle-end/121000 */
/* { dg-do run } */
/* { dg-options "-O" } */

#include "builtin-object-size-common.h"

/* The parameter m must be const qualified to avoid the m is 
   marked as TREE_SIDE_EFFECTS in IR.
   The __builtin_dynamic_object_size will be folded as -1 by
   fold_builtin_object_size when m is NOT const qualified.  */

void
foo (int n, const int m)
{
  typedef int A[m];
  struct S { int n, m; A a[2]; A b[] __attribute__((counted_by (n))); } *p;
  p = __builtin_malloc (sizeof (struct S) + sizeof (A) * n);
  p->n = n;
  p->m = m;
  EXPECT (__builtin_dynamic_object_size (p->b, 1), sizeof (A) * n);
}

/* The parameter m1, m2 must be const qualified to avoid the m is 
   marked as TREE_SIDE_EFFECTS in IR.
   The __builtin_dynamic_object_size will be folded as -1 by
   fold_builtin_object_size when m1 or m2 is NOT const qualified.  */
void
foo_1 (int n, const int m1, const int m2)
{
  typedef int A[m1][m2];
  struct S { int n; A a[2]; A b[] __attribute__((counted_by (n))); } *p;
  p = __builtin_malloc (sizeof (struct S) + sizeof (A) * n);
  p->n = n;
  EXPECT (__builtin_dynamic_object_size (p->b, 1), sizeof (A) * n);
}

int
main ()
{
  foo (2, 10);
  foo_1 (2, 10, 20);
  return 0;
}
