/* { dg-do run } */
/* { dg-options "-fdump-tree-ealias -Wno-attributes" } */
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
static int __attribute__((always_inline))
foo (struct X *x)
{
  struct Y y = x->y;
  /* In the inlined instance the dereferenced pointer needs to point to i.  */
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

/* { dg-final { scan-tree-dump "y.* = { i }" "ealias" } } */
/* { dg-final { scan-tree-dump "y.*, points-to vars: { D..... }" "ealias" } } */
/* { dg-final { cleanup-tree-dump "ealias" } } */
