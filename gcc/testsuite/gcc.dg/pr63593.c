/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-vectorize" } */

int in[2 * 4][4];
int out[4];

void
foo (void)
{
  int sum;
  int i, j, k;
  for (k = 0; k < 4; k++)
    {
      sum = 1;
      for (j = 0; j < 4; j++)
	for (i = 0; i < 4; i++)
	  sum *= in[i + k][j];
      out[k] = sum;
    }
}
