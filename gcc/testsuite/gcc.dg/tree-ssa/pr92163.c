/* { dg-do compile } */
/* { dg-require-effective-target fopenacc } */
/* { dg-options "-O2 -fexceptions -fnon-call-exceptions -fopenacc" } */

void
xr (int *k7)
{
  int qa;

#pragma acc parallel
#pragma acc loop vector
  for (qa = 0; qa < 3; ++qa)
    if (qa % 2 != 0)
      k7[qa] = 0;
    else
      k7[qa] = 1;
}
