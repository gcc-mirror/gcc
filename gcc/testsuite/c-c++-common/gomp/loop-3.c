#ifdef __cplusplus
extern "C" {
#endif
int omp_get_thread_num (void);
#ifdef __cplusplus
}
#endif

void
f1 (int *a)
{
  int i;
  #pragma omp loop		/* { dg-error "'bind' clause not specified on a 'loop' construct not nested inside another OpenMP construct" } */
  for (i = 0; i < 64; i++)
    a[i] = i;
}

void
f2 (int *a)
{
  int i, j;
  #pragma omp parallel num_threads (4)
  {
    int j = omp_get_thread_num ();
    #pragma omp loop private (i) bind(teams)	/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
    for (i = 0; i < 64; i++)
      a[j * 64 + i] = i;
  }
  #pragma omp critical
  {
    #pragma omp loop lastprivate (i) bind(teams)/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
    for (i = 0; i < 64; i++)
      a[i] = i;
  }
  #pragma omp master
  {
    #pragma omp loop bind(teams)		/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
    for (i = 0; i < 64; i++)
      a[i] = i;
  }
  #pragma omp sections
  {
    #pragma omp loop bind(teams) lastprivate(i)	/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
    for (i = 0; i < 64; i++)
      a[i] = i;
  }
  #pragma omp single
  {
    #pragma omp loop bind(teams)		/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
    for (i = 0; i < 64; i++)
      a[i] = i;
  }
  #pragma omp task
  {
    #pragma omp loop bind(teams)		/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
    for (i = 0; i < 64; i++)
      a[i] = i;
  }
  #pragma omp taskgroup
  {
    #pragma omp loop bind(teams)		/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
    for (i = 0; i < 64; i++)
      a[i] = i;
  }
  #pragma omp teams
  {
    #pragma omp distribute
    for (j = 0; j < 64; ++j)
      {
	#pragma omp loop bind(teams)		/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
	for (i = 0; i < 64; i++)
	  a[64 * j + i] = i;
      }
  }
  #pragma omp for
  for (j = 0; j < 64; ++j)
    {
      #pragma omp loop bind(teams)		/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
      for (i = 0; i < 64; i++)
	a[64 * j + i] = i;
    }
  #pragma omp parallel
  #pragma omp loop
  for (j = 0; j < 64; ++j)
    {
      #pragma omp loop bind(teams)		/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
      for (i = 0; i < 64; i++)
	a[64 * j + i] = i;
    }
  #pragma omp loop bind(thread)
  for (j = 0; j < 64; ++j)
    {
      #pragma omp loop bind(teams)		/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
      for (i = 0; i < 64; i++)
	a[64 * j + i] = i;
    }
  #pragma omp loop bind(parallel)
  for (j = 0; j < 64; ++j)
    {
      #pragma omp loop bind(teams)		/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
      for (i = 0; i < 64; i++)
	a[64 * j + i] = i;
    }
  #pragma omp for ordered
  for (j = 0; j < 64; ++j)
    {
      #pragma omp ordered threads
      #pragma omp loop bind(teams)		/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
      for (i = 0; i < 64; i++)
	a[64 * j + i] = i;
    }
  #pragma omp simd
  for (j = 0; j < 64; ++j)
    {
      #pragma omp loop bind(teams)		/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
      for (i = 0; i < 64; i++)
	a[64 * j + i] = i;
    }
  #pragma omp taskloop
  for (j = 0; j < 64; ++j)
    {
      #pragma omp loop bind(teams)		/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
      for (i = 0; i < 64; i++)
	a[64 * j + i] = i;
    }
  #pragma omp target
  {
    #pragma omp loop bind(teams)		/* { dg-error "'bind\\(teams\\)' on a 'loop' region not strictly nested inside of a 'teams' region" } */
    for (i = 0; i < 64; i++)
      a[i] = i;
  }
}

void
f3 (int *a)
{
  int i, j;
  #pragma omp simd
  for (j = 0; j < 64; j++)
    {
      #pragma omp loop bind(parallel)		/* { dg-error "'bind\\(parallel\\)' on a 'loop' construct nested inside 'simd' construct" } */
      for (i = 0; i < 64; i++)
	a[64 * j + i] = i;
    }
}
