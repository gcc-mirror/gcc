/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1-details" } */

void f(char *p, int n)
{
  for (int i = 0; i < n; ++i)
    *p = 0;  /* Removed by DSE.  */
  *p = 1;
}

void g(char *p, int n)
{
  int i = 0;
  do
    *p = 0;  /* Not yet removed by DSE.  */
  while (++i < n);
  *p = 1;
}


/* { dg-final { scan-tree-dump-times "Deleted dead store" 2 "dse1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "Deleted dead store" 1 "dse1" } } */
