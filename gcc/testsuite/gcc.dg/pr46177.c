/* { dg-do compile } */
/* { dg-options "-O -fno-tree-copy-prop -ftree-loop-distribution" } */

extern int A[];
extern int B[];

void
foo (int j, int c)
{
  int i;

  if (c)
    for (i = 0; i < j; i++)
      A[i] = B[i] = 0;

  for (i = 0; i < j; i++)
    A[i] = B[i];
}
