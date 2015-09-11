/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dse1" } */

void foo (int *p, int b)
{
  if (b)
    *p = 1;
  *p = 0;
}

/* { dg-final { scan-tree-dump-times "\\\*p" 1 "dse1" } } */
