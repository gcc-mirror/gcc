/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-sra -fipa-pta -fdump-ipa-pta2-details" } */

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
static int __attribute__((noinline))
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

/* { dg-final { scan-ipa-dump "y.\[0-9\]*\\\+\[0-9\]* = { i }" "pta2" } } */
