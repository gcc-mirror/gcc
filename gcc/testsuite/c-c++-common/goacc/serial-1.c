// See also 'kernels-1.c', 'parallel-1.c'.

int
serial_empty (void)
{
#pragma acc serial
  ;

  return 0;
}

int
serial_eternal (void)
{
#pragma acc serial
  {
    while (1)
      ;
  }

  return 0;
}

int
serial_noreturn (void)
{
#pragma acc serial
  __builtin_abort ();

  return 0;
}

int
serial_clauses (void)
{
  int a, b[100];

#pragma acc serial firstprivate (a, b)
  ;

  return 0;
}
