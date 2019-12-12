
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct a {int v1;
          int v2;};
struct b {int array[0]; struct a a[];};
union c {struct b b;};

int
test (struct b *bptr1, union c *cptr, int i, int j)
{
  bptr1->a[i].v1=123;
  cptr->b.a[j].v2=1;
  return bptr1->a[i].v1;
}
int
test2 (struct b *bptr1, union c *cptr, int i, int j)
{
  bptr1->a[i].v1=124;
  cptr->b.a[j].v1=1;
  return bptr1->a[i].v1;
}
/* { dg-final { scan-tree-dump-times "return 123" 1 "optimized"} } */
/* { dg-final { scan-tree-dump-not "return 124" "optimized"} } */
