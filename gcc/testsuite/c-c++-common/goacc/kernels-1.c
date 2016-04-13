int
kernels_empty (void)
{
#pragma acc kernels
  ;

  return 0;
}

int
kernels_eternal (void)
{
#pragma acc kernels
  {
    while (1)
      ;
  }

  return 0;
}

int
kernels_noreturn (void)
{
#pragma acc kernels
  __builtin_abort ();

  return 0;
}


float b[10][15][10];

void
kernels_loop_ptr_it (void)
{
  float *i;

#pragma acc kernels
  {
#pragma acc loop
    for (i = &b[0][0][0]; i < &b[0][0][10]; i++)
      ;
  }
}
