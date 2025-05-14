/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model" } */

double mat[100][8];

double
slp_reduc_plus (int n)
{
  double tmp = 0.0;
  for (int i = 0; i < n; i++)
    {
      tmp = tmp + mat[i][0];
      tmp = tmp + mat[i][1];
      tmp = tmp + mat[i][2];
      tmp = tmp + mat[i][3];
      tmp = tmp + mat[i][4];
      tmp = tmp + mat[i][5];
      tmp = tmp + mat[i][6];
      tmp = tmp + mat[i][7];
    }
  return tmp;
}

/* { dg-final { scan-assembler-times {\tfadda\td[0-9]+, p[0-7], d[0-9]+, z[0-9]+\.d} 4 } } */
