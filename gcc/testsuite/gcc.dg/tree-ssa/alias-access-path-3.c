/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */
struct a {int v1;
	  int v2;};
struct b {struct a a[0];};

int
test (struct b *bptr1, struct b *bptr2, int i, int j)
{
  bptr1->a[i].v1=123;
  bptr2->a[j].v2=1;
  return bptr1->a[i].v1;
}
int
test2 (struct b *bptr1, struct b *bptr2, int i, int j)
{
  bptr1->a[i].v1=123;
  bptr2->a[j].v1=1;
  return bptr1->a[i].v1;
}
/* test should be optimized, while test2 should not.  */
/* { dg-final { scan-tree-dump-times "return 123" 1 "fre1"} } */
