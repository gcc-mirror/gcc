/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx -mtune=generic" } */

double val1[4][2], val2[4][2], chk[4][2];

void
foo (void)
{
  int i, j;
  for (i = 0; i < 4; i++)
    {
      double tmp = 0;
      for (j = 0; j < 2; j++)
       tmp += val1[i][j] * val2[i][j];
      for (j = 0; j < 2; j++)
       chk[i][j] = tmp;
    }
}

float val1f[8][2], val2f[8][2], chkf[8][2];

void
foof (void)
{
  int i, j;
  for (i = 0; i < 8; i++)
    {
      float tmp = 0;
      for (j = 0; j < 2; j++)
       tmp += val1f[i][j] * val2f[i][j];
      for (j = 0; j < 2; j++)
       chkf[i][j] = tmp;
    }
}
