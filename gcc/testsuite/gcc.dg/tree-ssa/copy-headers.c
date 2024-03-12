/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-ch2-details" } */

extern int foo (int);

void bla (void)
{
  int i, n = foo (0);

  for (i = 0; i < n; i++)
    foo (i);
}

/* There should be a header duplicated.  */
/* { dg-final { scan-tree-dump-times "Duplicating header of the" 1 "ch2"} } */
