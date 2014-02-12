/* { dg-do compile } */
/* { dg-options "-fcilkplus -fopenmp" } */
/* { dg-require-effective-target fopenmp } */

int *a, *b, c;

void foo()
{
#pragma simd
  for (int i=0; i < 1000; ++i)
    {
      a[i] = b[i];
      if (c == 5)
	return; /* { dg-error "invalid branch to/from a Cilk Plus structured block" } */
    }
}

void bar()
{
#pragma simd
  for (int i=0; i < 1000; ++i)
    {
    lab:
      a[i] = b[i];
    }
  if (c == 6)
    goto lab; /* { dg-error "invalid entry to Cilk Plus structured block" } */
}

void baz()
{
  bad1:
  #pragma omp parallel
    goto bad1; /* { dg-error "invalid branch to/from an OpenMP structured block" } */

  goto bad2; /* { dg-error "invalid entry to OpenMP structured block" } */
  #pragma omp parallel
    {
      bad2: ;
    }

  #pragma omp parallel
    {
      int i;
      goto ok1;
      for (i = 0; i < 10; ++i)
	{ ok1: break; }
    }
}
