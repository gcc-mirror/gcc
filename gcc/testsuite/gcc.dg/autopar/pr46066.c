/* PR tree-optimization/46066 */
/* { dg-do compile } */
/* { dg-options "-fcompare-debug -O -ftree-parallelize-loops=4" } */

void
parloop (int N)
{
  int i, j, ii;
  int x[400][10][400];
  for (ii = 0; ii < N; ii++)
    for (i = 0; i < N; i++)
      for (j = 0; j < N; j++)
	x[i][j][ii] = 3;
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      if (x[i][j][0] != 3)
	__builtin_abort ();
}
