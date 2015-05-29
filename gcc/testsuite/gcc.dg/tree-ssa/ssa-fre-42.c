/* { dg-do run } */
/* { dg-require-alias "" } */
/* { dg-options "-O -fdump-tree-fre1" } */

extern void abort (void);

struct X { int a[128]; };
static const struct X a = { 0, 1, 2, 3 };
/* Prevent gimplify_modify_expr_rhs / gimplify_init_constructor from
   expanding the aggregate copy below inline.  */
static const struct X A __attribute__((alias("a")));
struct X *q;
int __attribute__((noinline))
foo ()
{
  struct X b = A;
  int *p = &b.a[2];
  /* Prevent SRA from decomposing b.  */
  q = &b;
  return *p;
}

int main()
{
  if (foo() != 2)
    abort ();
  return 0;
}

/* Verify the aggregate copy we want to look through is still in place.  */
/* { dg-final { scan-tree-dump "b = A;" "fre1" } } */
/* Verify we have propagated the element read all the way to the return.  */
/* { dg-final { scan-tree-dump "return 2" "fre1" } } */
