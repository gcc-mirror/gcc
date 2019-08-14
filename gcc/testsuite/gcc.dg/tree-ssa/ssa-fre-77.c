/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

int foo (int *p, int *q)
{
  int x;
  *p = 1;
  x = *p;
  *q = x;
  return *p;
}

/* { dg-final { scan-tree-dump "return 1;" "fre1" { xfail { ! natural_alignment_32 } } } } */
