// { dg-do compile }

void foo()
{
  int i, j;

  #pragma omp for
  for (i = 0; i < 10; ++i)
    break;			// { dg-error "break" }

  bad1:				// { dg-error "jump to label" }
  #pragma omp for
  for (i = 0; i < 10; ++i)
    goto bad1;			// { dg-error "from here|exits OpenMP" }

  goto bad2;			// { dg-error "from here" }
  #pragma omp for
  for (i = 0; i < 10; ++i)
    {
      bad2: ;			// { dg-error "jump|enters OpenMP" }
    }

  #pragma omp for
  for (i = 0; i < 10; ++i)
    for (j = 0; j < 10; ++j)
      if (i == j)
	break;

  #pragma omp for
  for (i = 0; i < 10; ++i)
    continue;
}
