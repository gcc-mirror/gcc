/* PR tree-optimization/30565  */

/* { dg-do compile } */
/* { dg-options "-O1 -ftree-pre -ftree-loop-linear" } */

static double snrdef[32];
void psycho_n1(double ltmin[2][32], int stereo)
{
  int i, k;

  for (k = 0; k < stereo; k++)
    for (i = 0; i < 32; i++)
      ltmin[k][i] = snrdef[i];
}
