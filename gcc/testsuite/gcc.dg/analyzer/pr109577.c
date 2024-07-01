/* C only: C++ exceptions cause a malloc leak after "safer" returns.
   Therefore this test has been duplicated as
   c-c++-common/analyzer/pr109577-noexcept.c  */

void *malloc (__SIZE_TYPE__);

double *
unsafe (__SIZE_TYPE__ n)
{
  return (double *) malloc (n * sizeof (double));
}

double *
safer (__SIZE_TYPE__ n)
{
  __SIZE_TYPE__ nbytes;
  if (__builtin_mul_overflow (n, sizeof (double), &nbytes))
    return 0;
  return (double *) malloc (nbytes); /* Exceptions enabled cause a leak here. */
}
