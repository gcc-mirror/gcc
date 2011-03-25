/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details" } */

int *q;
void __attribute__((noinline))
bar (void)
{
  *q = 1;
}
int foo(int which_p)
{
  int x = 0;
  int *i,*j;
  int **p;
  if (which_p)
    p = &i;
  else
    p = &j;
  *p = &x;
  bar ();
  return x;
}

/* { dg-final { scan-tree-dump "Replaced x with 0" "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
