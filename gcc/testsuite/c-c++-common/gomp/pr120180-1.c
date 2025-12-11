/* { dg-do compile } */
// { dg-additional-options "-Wno-deprecated-openmp" }
/* This test case checks that the inner metadirective is accepted as intervening
   code since it resolves to 'omp nothing'.  */

int main()
{
  int blksize = 15000;
  double *qq;
  int i, k, nq;

  #pragma omp metadirective when(user={condition(0)}: target teams distribute parallel for collapse(2) map(qq[ :0]) private(i)) \
                            when(user={condition(0)}: target teams distribute parallel for map(qq[ :0]) private(i)) \
                            when(user={condition(1)}: target teams loop collapse(2) map(qq[ :0]) private(i))
  for(k=0; k<blksize; k++)
    {
#pragma omp metadirective when(user={condition(0)}: simd) default()
      for (i=0; i<nq; i++)
        qq[k*nq + i] = 0.0;
    }
  return 0;
}
