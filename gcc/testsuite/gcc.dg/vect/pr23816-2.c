/* { dg-do compile } */
/* { dg-require-effective-target vect_condition } */

void
foo (double a[32], double b[2][32])
{
  int i;
  for (i = 0; i < 32; i++)
    a[i] = (b[0][i] > b[1][i]) ? b[0][i] : b[1][i];
}

