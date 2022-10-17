void foo (void);
int v;
#ifdef __cplusplus
extern "C" {
#endif
int omp_get_thread_num (void);
int omp_get_num_threads (void);
int omp_target_is_present (const void *, int);
int omp_get_cancellation (void);
#ifdef __cplusplus
}
#endif

void
f1 (int *a)
{
  int i;
  #pragma omp simd order(concurrent)
  for (i = 0; i < 64; i++)
    {
      int j;
      #pragma omp loop
      for (j = 0; j < 64; j++)
	a[64 * i + j] = i + j;
    }
}

void
f2 (int *a)
{
  int i;
  #pragma omp for simd order(concurrent)
  for (i = 0; i < 64; i++)
    {
      int j;
      #pragma omp loop
      for (j = 0; j < 64; j++)
	a[64 * i + j] = i + j;
    }
}

void
f3 (int *a)
{
  int i;
  #pragma omp for order(concurrent)
  for (i = 0; i < 64; i++)
    {
      int j;
      #pragma omp loop
      for (j = 0; j < 64; j++)
	a[64 * i + j] = i + j;
    }
}

void
f4 (int *a)
{
  int i;
  #pragma omp loop order(concurrent) bind(parallel)
  for (i = 0; i < 64; i++)
    {
      #pragma omp parallel
      foo ();
    }
  #pragma omp loop order(concurrent) bind(parallel)
  for (i = 0; i < 64; i++)
    {
      int j;
      #pragma omp simd
      for (j = 0; j < 64; j++)
	a[64 * i + j] = i + j;
    }
  #pragma omp loop order(concurrent) bind(parallel)
  for (i = 0; i < 64; i++)
    {
      int j;
      #pragma omp loop
      for (j = 0; j < 64; j++)
	a[64 * i + j] = i + j;
    }
  #pragma omp loop order(concurrent) bind(parallel)
  for (i = 0; i < 64; i++)
    {
      #pragma omp critical		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" } */
      foo ();
    }
  #pragma omp loop order(concurrent) bind(parallel)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered simd		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" } */
      foo ();
    }
  #pragma omp loop order(concurrent) bind(parallel)
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" } */
      v++;
    }
  #pragma omp loop order(concurrent) bind(parallel)
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic read		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" "" { target c++ } } */
      a[i] = v;				/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" "" { target c } } */
    }
  #pragma omp loop order(concurrent) bind(parallel)
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic write		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" "" { target c++ } } */
      v = a[i];				/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" "" { target c } } */
    }
  #pragma omp loop order(concurrent) bind(parallel)
  for (i = 0; i < 64; i++)
    a[i] += omp_get_thread_num ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp loop order(concurrent) bind(parallel)
  for (i = 0; i < 64; i++)
    a[i] += omp_get_num_threads ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp loop order(concurrent) bind(parallel)
  for (i = 0; i < 64; i++)
    a[i] += omp_target_is_present (a + i, 0);	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp loop order(concurrent) bind(parallel)
  for (i = 0; i < 64; i++)
    a[i] += omp_get_cancellation ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
}

void
f5 (int *a)
{
  int i;
  #pragma omp parallel
  {
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      #pragma omp parallel
      foo ();
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      int j;
      #pragma omp simd
      for (j = 0; j < 64; j++)
	a[64 * i + j] = i + j;
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      int j;
      #pragma omp loop
      for (j = 0; j < 64; j++)
	a[64 * i + j] = i + j;
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      #pragma omp critical		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" } */
      foo ();
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered simd		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" } */
      foo ();
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" } */
      v++;
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic read		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" "" { target c++ } } */
      a[i] = v;				/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" "" { target c } } */
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic write		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" "" { target c++ } } */
      v = a[i];				/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" "" { target c } } */
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      #pragma omp master		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" } */
      foo ();
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      #pragma omp masked		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" } */
      foo ();
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      #pragma omp scope			/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" } */
      foo ();
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    a[i] += omp_get_thread_num ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp loop
  for (i = 0; i < 64; i++)
    a[i] += omp_get_num_threads ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp loop
  for (i = 0; i < 64; i++)
    a[i] += omp_target_is_present (a + i, 0);	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp loop
  for (i = 0; i < 64; i++)
    a[i] += omp_get_cancellation ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  }
}

void
f6 (int *a)
{
  int i;
  #pragma omp master
  {
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      #pragma omp parallel
      foo ();
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      int j;
      #pragma omp simd
      for (j = 0; j < 64; j++)
	a[64 * i + j] = i + j;
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      int j;
      #pragma omp loop
      for (j = 0; j < 64; j++)
	a[64 * i + j] = i + j;
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      #pragma omp critical		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" } */
      foo ();
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered simd		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" } */
      foo ();
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" } */
      v++;
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic read		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" "" { target c++ } } */
      a[i] = v;				/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" "" { target c } } */
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic write		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" "" { target c++ } } */
      v = a[i];				/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a 'loop' region" "" { target c } } */
    }
  #pragma omp loop
  for (i = 0; i < 64; i++)
    a[i] += omp_get_thread_num ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp loop
  for (i = 0; i < 64; i++)
    a[i] += omp_get_num_threads ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp loop
  for (i = 0; i < 64; i++)
    a[i] += omp_target_is_present (a + i, 0);	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp loop
  for (i = 0; i < 64; i++)
    a[i] += omp_get_cancellation ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  }
}

