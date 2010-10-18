/* { dg-do run } */
/* { dg-options "-O -fdump-tree-alias-details" } */

int *i;
void __attribute__((noinline))
foo (void)
{
  *i = 1;
}
int __attribute__((noinline))
bar(int local_p)
{
  int x = 0;
  int *j;
  int **p;
  if (local_p)
    p = &j;
  else
    p = &i;
  *p = &x;  /* This makes x escape.  */
  foo ();
  return x;
}
extern void abort (void);
int main()
{
  int k = 2;
  i = &k;
  if (bar (1) != 0 || k != 1)
    abort ();
  if (bar (0) != 1)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "ESCAPED, points-to non-local, points-to NULL, points-to vars: { x }" "alias" } } */
/* { dg-final { cleanup-tree-dump "alias" } } */
