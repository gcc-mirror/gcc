/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_double } */

void foo (float *__restrict__ out, double *__restrict__ a,
          double *__restrict__ b0, double *__restrict__ b1,
          double *__restrict__ b2, double *__restrict__ b3,
          double *__restrict__ b4, double *__restrict__ b5)
{
  out[0] = (float)(a[0] + *b0);
  out[1] = (float)(a[1] + *b1);
  out[2] = (float)(a[2] + *b2);
  out[3] = (float)(a[3] + *b3);
  out[4] = (float)(a[4] + *b4);
  out[5] = (float)(a[5] + *b5);
}

/* { dg-final { scan-tree-dump "Splitting SLP group at stmt 4" "slp2" } } */
