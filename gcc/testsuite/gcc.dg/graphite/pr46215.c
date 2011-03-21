/* { dg-do compile } */
/* { dg-options "-O -fstrict-overflow -fno-tree-ch -ftree-loop-linear" } */

extern int y[], x1[], x2[];
void
foo (long n)
{
  int i, j;
  for (i = 0; i < n; i++)
    for (j = 0; j < n; j += 2)
      y[i] = x1[i + j] * x2[i + j];
}
