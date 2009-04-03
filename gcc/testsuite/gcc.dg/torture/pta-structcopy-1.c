/* { dg-do run } */
/* { dg-options "-fno-tree-sra -fdump-tree-alias" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

struct X
{
  long l1;
  struct Y
    {
      long l2;
      int *p;
    } y;
};
int i;
static int
foo (struct X *x)
{
  struct Y y = x->y;
  *y.p = 0;
  i = 1;
  return *y.p;
}
extern void abort (void);
int main()
{
  struct X x;
  x.y.p = &i;
  if (foo(&x) != 1)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "points-to vars: { i }" "alias" } } */
/* { dg-final { cleanup-tree-dump "alias" } } */
