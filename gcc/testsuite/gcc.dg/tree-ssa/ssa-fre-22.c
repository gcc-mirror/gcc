/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

int i;
int foo (void)
{
  int j;
  i = j;
  return i;
}

/* We should eliminate the redundant load of i.  */

/* { dg-final { scan-tree-dump-not "= i;" "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
