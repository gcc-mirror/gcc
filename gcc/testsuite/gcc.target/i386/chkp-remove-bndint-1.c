/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "bndint" "optimized" } } */

struct S
{
  int a;
  int b;
  int c;
};

int test (struct S *ps)
{
  int *pi = &ps->b;
  return *pi;
}
