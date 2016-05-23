/* { dg-do compile } */

void
foo (int x, int y)
{
  bad1:
  #pragma omp target data map(tofrom: y)
    goto bad1; // { dg-error "invalid branch to/from OpenMP structured block" }

  goto bad2; // { dg-error "invalid entry to OpenMP structured block" }
  #pragma omp target data map(tofrom: y)
    {
      bad2: ;
    }

  #pragma omp target data map(tofrom: y)
    {
      int i;
      goto ok1;
      for (i = 0; i < 10; ++i)
	{ ok1: break; }
    }

  switch (x) // { dg-error "invalid entry to OpenMP structured block" }
  {
  #pragma omp target data map(tofrom: y) // { dg-warning "statement will never be executed" }
    { case 0:; }
  }
}
