/* C only: C++ exceptions cause a malloc leak after "safer" returns.
   Therefore this test has been duplicated as
   c-c++-common/analyzer/pr109577-noexcept.c  */

void *malloc (unsigned long);

double *
unsafe (unsigned long n)
{
  return (double *) malloc (n * sizeof (double));
}

double *
safer (unsigned long n)
{
  unsigned long nbytes;
  if (__builtin_mul_overflow (n, sizeof (double), &nbytes))
    return 0;
  return (double *) malloc (nbytes); /* Exceptions enabled cause a leak here. */
}
