/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1-details" } */

_Bool g(void);

void f(int x)
{
  char arr[x];

  arr[0] = 0;

  if (g())
    __builtin_abort();
}

/* { dg-final { scan-tree-dump "Deleted dead store" "dse1" } } */
