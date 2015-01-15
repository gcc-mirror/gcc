// { dg-do compile }

void foo()
{
  #pragma omp ordered
    {
      return; // { dg-error "invalid branch to/from OpenMP structured block" }
    }
}
