// { dg-do compile }

void foo()
{
  #pragma omp critical
    {
      return; // { dg-error "invalid branch to/from OpenMP structured block" }
    }
}
