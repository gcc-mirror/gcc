/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */
/* { dg-additional-options "-mavx" { target { i?86-*-* x86_64-*-* } } } */

void fn1(double **matrix, int column, int row, int n)
{
  int k;
  for (k = 0; k < n; k++)
    if (matrix[row][k] != matrix[column][k])
      {
	matrix[column][k] = -matrix[column][k];
	matrix[row][k] = matrix[row][k] - matrix[column][k];
      }
}
