/* PR rtl-optimization/15139 */
/* { dg-do compile } */
/* { dg-options "-O2 -funroll-loops" } */

void
foo (double **a, double **z)
{
  long int i, j;
  double d = -1.0;

  for (i = 0; i < 6; i++)
    for (j = 0; j < 5; j++)
      d = z[i][j] > d ? z[i][j] : d;

  for (i = 0; i < 6; i++)
    for (j = 0; j < 5; j++)
      z[i][j] /= d;

  for (i = 0; i < 5; i++)
    a[i][j] = z[i][j];
}
