/* { dg-do compile } */

void
foo (int x)
{
  bad1:
  #pragma omp target
    goto bad1; // { dg-error "invalid branch to/from OpenMP structured block" }

  goto bad2; // { dg-error "invalid entry to OpenMP structured block" }
  #pragma omp target
    {
      bad2: ;
    }

  #pragma omp target
    {
      int i;
      goto ok1;
      for (i = 0; i < 10; ++i)
	{ ok1: break; }
    }

  switch (x) // { dg-error "invalid entry to OpenMP structured block" }
  {
  #pragma omp target // { dg-warning "statement will never be executed" }
    { case 0:; }
  }
}
