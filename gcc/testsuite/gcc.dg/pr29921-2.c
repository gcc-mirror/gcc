/* With -ffast-math, the latice value for sum2 used to change from NaN to
   VARYING, in turn causing the lattice value of sum1 * sum2 change from
   NaN to 0 (since sum1 is believed to be 0 at that moment, and
   0 * VARYING = 0 with -ffast-math), which caused an ICE.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

int
foo (float *array, int end)
{
  int i;
  float sum1, sum2;

  sum2 = 0;
  for (i = 0; i < end; i++)
    sum2 = sum2+array[i];
  sum2 = 1./sum2;
  sum1 = 0.;
  for (i = 0; i < end; i++)
    sum1 = sum1+array[i];
  sum1 = sum1 * sum2;
  if (-10.0 / sum1 < 5.E-5)
    end = 0;
  return end;
}

