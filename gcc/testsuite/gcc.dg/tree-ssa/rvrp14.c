/* adapted from PR tree-optimization/21294
   In this testcase, noticing that &p->i cannot be null
   allows us to eliminate the second "if" statement.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dominator-opts -fdump-tree-rvrp" } */

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

/* { dg-final { scan-tree-dump "Branch rewritten"  "rvrp"} } */
