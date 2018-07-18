/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

int i;
int foo (int b)
{
  int j;
  i = 1;
  if (b)
    j = i;
  return i - j;
}

/* { dg-final { scan-tree-dump "return 0;" "fre1" } } */
