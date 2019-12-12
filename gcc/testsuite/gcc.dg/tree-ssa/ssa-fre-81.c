/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1-details" } */

struct a
{
  int foo,bar;
};
struct b
{
  struct a a[10];
};
struct b b, *bptr=&b, *bptr2=&b;
int j;
int i;
int n=1;

int
main ()
{
  int jj=j;
  bptr2->a[jj].bar = 0;
  for (int i=0; i<n; i++)
    bptr->a[i].foo=1;
  if (!__builtin_constant_p (bptr2->a[jj].bar == 0))
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "Replaced __builtin_constant_p \\\(\[^)\]*\\\) with 1" "fre1" } } */
