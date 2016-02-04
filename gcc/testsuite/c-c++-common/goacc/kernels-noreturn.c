int
main (void)
{

#pragma acc kernels
  {
    __builtin_abort ();
  }

  return 0;
}

