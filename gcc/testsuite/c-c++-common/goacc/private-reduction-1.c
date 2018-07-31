int
reduction ()
{
  int i, r;

  #pragma acc parallel
  #pragma acc loop private (r) reduction (+:r)
  for (i = 0; i < 100; i++)
    r += 10;

  return r;
}
