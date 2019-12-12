/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-O2 -fdump-tree-dse1-details" } */

int i;
int *foo (int bogus, int n)
{
  int a[n];
  a[2] = bogus; /* Should elide this store since a cannot escape.  */
  int *p;
  if (bogus)
    p = &a[2];
  else
    p = &i;
  return p;         /* { dg-warning "\\\[-Wreturn-local-addr" } */
}

/* { dg-final { scan-tree-dump "Deleted dead store" "dse1" } } */
