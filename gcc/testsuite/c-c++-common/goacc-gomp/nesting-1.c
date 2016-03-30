void
f_acc_data (void)
{
#pragma acc data
  {
    int i;
#pragma omp atomic write
    i = 0;
  }
}

void
f_acc_kernels (void)
{
#pragma acc kernels
  {
    int i;
#pragma omp atomic write
    i = 0;
  }
}

#pragma acc routine vector
void
f_acc_loop (void)
{
  int i;

#pragma acc loop
  for (i = 0; i < 2; ++i)
    {
#pragma omp atomic write
      i = 0;
    }
}

void
f_acc_parallel (void)
{
#pragma acc parallel
  {
    int i;
#pragma omp atomic write
    i = 0;
  }
}
