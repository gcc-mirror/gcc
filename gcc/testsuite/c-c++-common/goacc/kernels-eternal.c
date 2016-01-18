int
main (void)
{
#pragma acc kernels
  {
    while (1)
      ;
  }

  return 0;
}
