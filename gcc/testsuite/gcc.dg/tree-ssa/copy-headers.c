/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-ch-details" } */

extern int foo (int);

void bla (void)
{
  int i, n = foo (0);

  for (i = 0; i < n; i++)
    foo (i);
}

/* There should be a header scheduled for duplication.  */
/* { dg-final { scan-tree-dump-times "Scheduled" 1 "ch"} } */
