/* { dg-do compile } */
/* { dg-options "-O -fno-tree-copy-prop -ftree-loop-distribution" } */

int A[30], B[30];

void
foo (int j)
{
  int i, k;
  for (k = 0; k < 10; k++)
    if (j)
      {
	for (; j < k; j++)
	  ;
	for (i = 0; i < k; i++)
	  A[i] = B[i] = 0;
      }
}
