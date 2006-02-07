/* PR tree-optimization/21294
   VRP did not notice that an address of the form &p->i is nonnull
   when p is known to be nonnull.  In this testcase, noticing that
   allows us to eliminate the second "if" statement.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dominator-opts -fdump-tree-vrp1-details" } */

struct f {
  int i;
};

int
foo (struct f *p)
{
  if (p != 0)
    if (&p->i != 0)
      return 123;
  return 0;
}

/* { dg-final { scan-tree-dump-times "Folding predicate" 1 "vrp1"} } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
