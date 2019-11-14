/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

double mat[100][2];

double
slp_reduc_plus (int n)
{
  double tmp = 0.0;
  for (int i = 0; i < n; i++)
    {
      tmp = tmp + mat[i][0];
      tmp = tmp + mat[i][1];
    }
  return tmp;
}

/* { dg-final { scan-assembler-times {\tfadda\td[0-9]+, p[0-7], d[0-9]+, z[0-9]+\.d\n} 1 } } */
