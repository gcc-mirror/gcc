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
      #pragma omp parallel		/* { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" } */
      foo ();
    }
  #pragma omp simd order(concurrent)
  for (i = 0; i < 64; i++)
    {
      int j;
      #pragma omp simd
      for (j = 0; j < 64; j++)
	a[64 * i + j] = i + j;
    }
  #pragma omp simd order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp critical		/* { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" } */
      foo ();
    }
  #pragma omp simd order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered simd		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" } */
      foo ();
    }
  #pragma omp simd order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" } */
      v++;
    }
  #pragma omp simd order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic read		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" "" { target c++ } } */
      a[i] = v;				/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" "" { target c } } */
    }
  #pragma omp simd order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic write		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" "" { target c++ } } */
      v = a[i];				/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" "" { target c } } */
    }
  #pragma omp simd order(concurrent)
  for (i = 0; i < 64; i++)
    a[i] += omp_get_thread_num ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp simd order(concurrent)
  for (i = 0; i < 64; i++)
    a[i] += omp_get_num_threads ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp simd order(concurrent)
  for (i = 0; i < 64; i++)
    a[i] += omp_target_is_present (a + i, 0);	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp simd order(concurrent)
  for (i = 0; i < 64; i++)
    a[i] += omp_get_cancellation ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
}

void
f2 (int *a)
{
  int i;
  #pragma omp for simd order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp parallel		/* { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" } */
      foo ();
    }
  #pragma omp for simd order(concurrent)
  for (i = 0; i < 64; i++)
    {
      int j;
      #pragma omp simd
      for (j = 0; j < 64; j++)
	a[64 * i + j] = i + j;
    }
  #pragma omp for simd order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp critical		/* { dg-error "OpenMP constructs other than 'ordered simd', 'simd', 'loop' or 'atomic' may not be nested inside 'simd' region" } */
      foo ();
    }
  #pragma omp for simd order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered simd		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" } */
      foo ();
    }
  #pragma omp for simd order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" } */
      v++;
    }
  #pragma omp for simd order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic read		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" "" { target c++ } } */
      a[i] = v;				/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" "" { target c } } */
    }
  #pragma omp for simd order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic write		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" "" { target c++ } } */
      v = a[i];				/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" "" { target c } } */
    }
  #pragma omp for simd order(concurrent)
  for (i = 0; i < 64; i++)
    a[i] += omp_get_thread_num ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp for simd order(concurrent)
  for (i = 0; i < 64; i++)
    a[i] += omp_get_num_threads ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp for simd order(concurrent)
  for (i = 0; i < 64; i++)
    a[i] += omp_target_is_present (a + i, 0);	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp for simd order(concurrent)
  for (i = 0; i < 64; i++)
    a[i] += omp_get_cancellation ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
}

void
f3 (int *a)
{
  int i;
  #pragma omp for order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp parallel
      foo ();
    }
  #pragma omp for order(concurrent)
  for (i = 0; i < 64; i++)
    {
      int j;
      #pragma omp simd
      for (j = 0; j < 64; j++)
	a[64 * i + j] = i + j;
    }
  #pragma omp for order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp critical		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" } */
      foo ();
    }
  #pragma omp for order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered simd		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" } */
      foo ();
    }
  #pragma omp for order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" } */
      v++;
    }
  #pragma omp for order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic read		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" "" { target c++ } } */
      a[i] = v;				/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" "" { target c } } */
    }
  #pragma omp for order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp atomic write		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" "" { target c++ } } */
      v = a[i];				/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" "" { target c } } */
    }
  #pragma omp for order(concurrent)
  for (i = 0; i < 64; i++)
    {
      #pragma omp task			/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" } */
      a[i]++;
    }
  #pragma omp for order(concurrent)
  for (i = 0; i < 64; i++)
    {
      int j;
      #pragma omp taskloop		/* { dg-error "OpenMP constructs other than 'parallel', 'loop' or 'simd' may not be nested inside a region with the 'order\\(concurrent\\)' clause" } */
      for (j = 0; j < 64; j++)
	a[64 * i + j] = i + j;
    }
  #pragma omp for order(concurrent)
  for (i = 0; i < 64; i++)
    a[i] += omp_get_thread_num ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_thread_num\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp for order(concurrent)
  for (i = 0; i < 64; i++)
    a[i] += omp_get_num_threads ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_num_threads\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp for order(concurrent)
  for (i = 0; i < 64; i++)
    a[i] += omp_target_is_present (a + i, 0);	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_target_is_present\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
  #pragma omp for order(concurrent)
  for (i = 0; i < 64; i++)
    a[i] += omp_get_cancellation ();	/* { dg-error "OpenMP runtime API call '\[^\n\r]*omp_get_cancellation\[^\n\r]*' in a region with 'order\\(concurrent\\)' clause" } */
}
