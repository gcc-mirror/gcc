#pragma omp declare target
void f0 (void);

void
f1 (void)
{
  int i;
  #pragma omp distribute dist_schedule(static) dist_schedule(static)	/* { dg-warning "too many 'dist_schedule' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
  #pragma omp distribute dist_schedule(static,2) dist_schedule(static,4) /* { dg-warning "too many 'dist_schedule' clauses" } */
  for (i = 0; i < 8; ++i)
    f0 ();
}
#pragma omp end declare target
