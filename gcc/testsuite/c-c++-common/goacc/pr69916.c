/* { dg-additional-options "-O2" } */

/* PR 69916, an loop determined to be empty sometime after omp-lower
   and before oacc-device-lower can evaporate leading to no GOACC_LOOP
   internal functions existing.  */

int
main (void)
{

#pragma acc parallel
  {
    int j = 0;
#pragma acc loop private (j)
    for (int i = 0; i < 10; i++)
      j++;
  }

  return 0;
}
