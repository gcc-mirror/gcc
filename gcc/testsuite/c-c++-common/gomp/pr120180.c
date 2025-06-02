/* { dg-do compile } */

/* This test used to ICE after erroring on the metadirective in the
   loop nest.  */

int main()
{
  int blksize = 15000;
  double *qq;
  int i, k, nq;

  #pragma omp metadirective when(user={condition(0)}: target teams distribute parallel for collapse(2) map(qq[:0]) private(i)) \
                            when(user={condition(0)}: target teams distribute parallel for map(qq[:0]) private(i)) \
                            when(user={condition(1)}: target teams loop collapse(2) map(qq[:0]) private(i))
  for(k=0; k<blksize; k++)
    {
#pragma omp metadirective when(user={condition(0)}: simd) default() // { dg-error "intervening code must not contain OpenMP directives" }
      for (i=0; i<nq; i++)
        qq[k*nq + i] = 0.0;
    }
  return 0;
}
