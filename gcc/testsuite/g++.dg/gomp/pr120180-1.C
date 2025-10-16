// { dg-do compile }
// { dg-additional-options "-std=c++11" }

// This test case checks that the inner metadirective is accepted as intervening
// code since it resolves to 'omp nothing'.

int main()
{
  constexpr int use_teams = 1;
  constexpr int use_simd = 0;
  
  int blksize = 15000;
  double *qq;
  int i, k, nq;

  #pragma omp metadirective when(user={condition(use_teams)}: teams distribute parallel for collapse(2)) \
                            otherwise(parallel for collapse(1))
  for(k=0; k<blksize; k++)
    {
      #pragma omp metadirective when(user={condition(use_simd)}: simd) \
                                otherwise(nothing)
      for (i=0; i<nq; i++)
        qq[k*nq + i] = 0.0;
    }
  return 0;
}
