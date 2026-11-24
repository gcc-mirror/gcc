/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_double } */

void foo1 (float *__restrict__ out,
           double *__restrict__ b0, double *__restrict__ b1,
           double *__restrict__ b2, double *__restrict__ b3,
           double *__restrict__ b4, double *__restrict__ b5)
{
  out[0] = (float)*b0;
  out[1] = (float)*b1;
  out[2] = (float)*b2;
  out[3] = (float)*b3;
  out[4] = (float)*b4;
  out[5] = (float)*b5;
}

void foo2 (float *__restrict__ out, float *__restrict__ a,
           float *__restrict__ b0, float *__restrict__ b1,
           float *__restrict__ b2, float *__restrict__ b3,
           float *__restrict__ b4, float *__restrict__ b5)
{
  out[0] = *b0 + a[0];
  out[1] = *b1 + a[1];
  out[2] = *b2 + a[2];
  out[3] = *b3 + a[3];
  out[4] = *b4 + a[4];
  out[5] = *b5 + a[5];
}

void foo3 (float *__restrict__ out,
           float *__restrict__ b0, float *__restrict__ b1,
           float *__restrict__ b2, float *__restrict__ b3,
           float *__restrict__ b4, float *__restrict__ b5)
{
  out[0] = *b0;
  out[1] = *b1;
  out[2] = *b2;
  out[3] = *b3;
  out[4] = *b4;
  out[5] = *b5;
}

/* { dg-final { scan-tree-dump-times "Splitting SLP group at stmt 4" 3 "slp2" } } */
