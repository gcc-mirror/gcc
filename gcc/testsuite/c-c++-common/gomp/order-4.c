int t;
#pragma omp threadprivate(t)

void
f1 (void)
{
  int i;
  #pragma omp simd order(concurrent)	/* { dg-message "note: enclosing region" } */
  for (i = 0; i < 64; i++)
    t++;	/* { dg-error "threadprivate variable 't' used in a region with 'order\\(concurrent\\)' clause" } */
}

void
f2 (void)
{
  int i;
  #pragma omp for simd order(concurrent)	/* { dg-message "note: enclosing region" } */
  for (i = 0; i < 64; i++)			/* { dg-message "note: enclosing region" "" { target c++ } } */
    t++;	/* { dg-error "threadprivate variable 't' used in a region with 'order\\(concurrent\\)' clause" } */
}

void
f3 (void)
{
  int i;
  #pragma omp for order(concurrent)	/* { dg-message "note: enclosing region" } */
  for (i = 0; i < 64; i++)
    t++;	/* { dg-error "threadprivate variable 't' used in a region with 'order\\(concurrent\\)' clause" } */
}
