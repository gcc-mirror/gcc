// { dg-do compile }

void foo()
{
  bad1:
  #pragma omp parallel
    goto bad1;			// { dg-error "invalid branch" }

  goto bad2;			// { dg-error "invalid entry" }
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
