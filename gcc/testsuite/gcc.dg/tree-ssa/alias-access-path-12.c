/* { dg-do compile } */
/* { dg-options "-O1 -fno-strict-aliasing -fdump-tree-optimized" } */

struct S
{
  int i;
  int j;
};
union U
{
  struct S a[10];
};
int
foo (union U *u, int n, int i, int j)
{
  u->a[i].i = 123;
  u->a[j].j = j;
  return u->a[i].i;
}
/* { dg-final { scan-tree-dump-times "return 123" 1 "optimized"} } */
