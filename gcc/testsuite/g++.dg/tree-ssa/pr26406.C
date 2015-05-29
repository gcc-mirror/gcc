/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fdelete-null-pointer-checks" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

int *f(int *b)
{
  int * a = new int[104];
  *a = 1;
  if (a == 0)
    return b;
  return a;
}

/* { dg-final { scan-tree-dump-not "if" "optimized" } } */
