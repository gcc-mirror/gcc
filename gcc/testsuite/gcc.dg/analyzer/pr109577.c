void *malloc (unsigned long);

double *
unsafe (unsigned long n)
{
  return malloc (n * sizeof (double));
}

double *
safer (unsigned long n)
{
  unsigned long nbytes;
  if (__builtin_mul_overflow (n, sizeof (double), &nbytes))
    return 0;
  return malloc (nbytes);
}
