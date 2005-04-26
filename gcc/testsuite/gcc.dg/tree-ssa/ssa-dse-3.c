/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1" } */

char Bool_Glob;
void f(void)
{
  Bool_Glob = 0;
  Bool_Glob = 1;
}

/* We should eliminate the first assignment to *p, but not the second.  */
/* { dg-final { scan-tree-dump-times "Bool_Glob = 0" 0 "dse1"} } */
/* { dg-final { scan-tree-dump-times "Bool_Glob = 1" 1 "dse1"} } */

/* { dg-final { cleanup-tree-dump "dse1" } } */

