/* { dg-do compile } */
/* { dg-options "-O -ftree-vectorize -fdump-tree-vect-details -fexceptions" } */

typedef __attribute__ ((const)) int (*bart) (void);

int foo (bart bar, int m)
{
  int i, j = 0;
  for (i = 0; i < m; i++)
    j += bar();
  return j;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
