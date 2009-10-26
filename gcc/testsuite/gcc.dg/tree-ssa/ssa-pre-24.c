/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */

void foo(int *p, double *x, int n)
{
  int i;
  for (i = 0; i < n; ++i)
    *(x + *p * i) = 0.0;
}

/* We should remove the unnecessary insertion of a phi-node and
   _not_ end up using the phi result for replacement *p.  */

/* { dg-final { scan-tree-dump-not "= prephitmp" "pre" } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
