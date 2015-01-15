// { dg-do compile }

void foo()
{
  #pragma omp master
    {
      goto bad1; // { dg-error "invalid branch to/from OpenMP structured block" }
    }

  #pragma omp master
    {
    bad1:
      return; // { dg-error "invalid branch to/from OpenMP structured block" }
    }
}
