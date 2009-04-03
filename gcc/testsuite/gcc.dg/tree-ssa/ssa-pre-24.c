/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */

void foo(int *p, double *x, int n)
{
  int i;
  for (i = 0; i < n; ++i)
    *(x + *p * i) = 0.0;
}

/* We should remove the unnecessary insertion of a phi-node and
   _not_ end up using the phi result for replacement *p.
   The issue here is that when PHI-translating the virtual operands
   we assign different value-numbers to the load.  Re-running VN
   after insertion or trying to be clever and doing this on the
   fly during PHI translation would solve this.  The next copyprop
   fixes this anyway.  */

/* { dg-final { scan-tree-dump-not "= prephitmp" "pre" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
