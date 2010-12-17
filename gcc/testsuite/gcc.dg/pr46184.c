/* { dg-do compile } */
/* { dg-options "-O -ftree-vectorize -fno-tree-copy-prop -fno-tree-dce" } */

extern int A[], B[];

void
foo (int z)
{
  int j, i;
  for (j = 0; j < 32; j++)
    {
      int curr_a = A[0];
      for (i = 0; i < 16; i++)
	curr_a = A[i] ? curr_a : z;
      B[j] = curr_a;
    }
}

