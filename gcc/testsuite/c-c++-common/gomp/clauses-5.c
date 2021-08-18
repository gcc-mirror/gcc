void
foo (int *p)
{
  int i, j = 0;
  #pragma omp parallel if (2, 1)		/* { dg-error "expected" } */
  ;
  #pragma omp parallel num_threads (3, 4)	/* { dg-error "expected" } */
  ;
  #pragma omp teams num_teams (4, 5)		/* { dg-error "expected" } */
  ;
  #pragma omp teams thread_limit (6, 7)		/* { dg-error "expected" } */
  ;
  #pragma omp for linear (j : 8, 9)		/* { dg-error "expected" } */
  for (i = 0; i < 30; i++)
    j += (8, 9);
  #pragma omp for schedule (static, 3, 4)	/* { dg-error "expected" } */
  for (i = 0; i < 30; i++)
    ;
  #pragma omp for collapse (1, 1)		/* { dg-error "expected" } */
  for (i = 0; i < 30; i++)
    ;
  #pragma omp for ordered (1, 1)		/* { dg-error "expected" } */
  for (i = 0; i < 30; i++)
    ;
  #pragma omp simd safelen (3, 4)		/* { dg-error "expected" } */
  for (i = 0; i < 30; i++)
    ;
  #pragma omp simd simdlen (4, 8)		/* { dg-error "expected" } */
  for (i = 0; i < 30; i++)
    ;
  #pragma omp simd aligned (p: 4, 8)		/* { dg-error "expected" } */
  for (i = 0; i < 30; i++)
    ;
  #pragma omp teams
  #pragma omp distribute dist_schedule (static, 6, 7) /* { dg-error "expected" } */
  for (i = 0; i < 30; i++)
    ;
  #pragma omp task final (8, 1)			/* { dg-error "expected" } */
  ;
  #pragma omp task priority (2, 3)		/* { dg-error "expected" } */
  ;
  #pragma omp taskloop grainsize (4, 5)		/* { dg-error "expected" } */
  for (i = 0; i < 30; i++)
    ;
  #pragma omp taskloop num_tasks (5, 6)		/* { dg-error "expected" } */
  for (i = 0; i < 30; i++)
    ;
  #pragma omp target device (5, 1)		/* { dg-error "expected" } */
  ;
  #pragma omp critical (baz) hint (2, 3)	/* { dg-error "expected" } */
  ;
  #pragma omp masked filter (3, 4)		/* { dg-error "expected" } */
  ;
}
