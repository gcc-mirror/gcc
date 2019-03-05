int a[32], b[32];

void
foo (void)
{
  int i, j;
  #pragma omp simd linear(i, j) collapse(2)	/* { dg-error "iteration variable 'i' should not be linear" } */
  for (i = 0; i < 32; ++i)			/* { dg-error "iteration variable 'j' should not be linear" "" { target *-*-* } .-1 } */
    for (j = 0; j < 32; ++j)
      a[i] += b[j];
}

void
bar (void)
{
  static int i, j;
  #pragma omp for simd linear(i, j) collapse(2)	/* { dg-error "iteration variable 'i' should not be linear" } */
  for (i = 0; i < 32; ++i)			/* { dg-error "iteration variable 'j' should not be linear" "" { target *-*-* } .-1 } */
    for (j = 0; j < 32; ++j)
      a[i] += b[j];
}
