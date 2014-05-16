/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce1" } */

struct S { unsigned f; };

int
foo ( struct S *p)
{
  int *q = (int *)&p->f;
  int i = *q;
  return i + p->f;
}


/* There should only be one load of p->f because FRE removes the redundancy
   by realizing it can cast the result of either to the other.  */
/* { dg-final { scan-tree-dump-times "= \[^\n\]*p_.\\\(D\\\)" 1 "cddce1" } } */
/* { dg-final { cleanup-tree-dump "cddce1" } } */

