/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-fre1" } */

struct a {int a1; int a2;};
struct b:a {};

struct b bvar,*bptr2;
int
test(void)
{
  struct a *bptr = &bvar;
  bptr->a2=0;
  bptr2->a1=1;
  return bptr->a2;
}
int
test2(void)
{
  struct b *bptr = &bvar;
  bptr->a2=0;
  bptr2->a1=1;
  return bptr->a2;
}
/* { dg-final { scan-tree-dump-times "return 0" 2 "fre1" } } */
