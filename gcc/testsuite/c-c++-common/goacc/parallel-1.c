int
parallel_empty (void)
{
#pragma acc parallel
  ;

  return 0;
}

int
parallel_eternal (void)
{
#pragma acc parallel
  {
    while (1)
      ;
  }

  return 0;
}

int
parallel_noreturn (void)
{
#pragma acc parallel
  __builtin_abort ();

  return 0;
}

int
parallel_clauses (void)
{
  int a, b[100];

#pragma acc parallel firstprivate (a, b)
  ;

  return 0;
}
