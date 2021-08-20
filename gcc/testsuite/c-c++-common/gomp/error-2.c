void
foo (int x, const char *msg1, const char *msg2)
{
  if (x == 0)
    {
      #pragma omp error at(execution)
    }
  else if (x == 1)
    {
      #pragma omp error severity (warning), at (execution)
    }
  else if (x == 2)
    {
      #pragma omp error at ( execution ) severity (fatal) message ("baz")
    }
  else if (x == 3)
    {
      #pragma omp error severity(warning) message (msg1) at(execution)
    }
  else
    {
      #pragma omp error message (msg2), at(execution), severity(fatal)
    }
}
