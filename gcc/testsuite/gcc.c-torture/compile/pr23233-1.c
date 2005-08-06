void foo (volatile long long *x)
{
  while (*x)
    {
      *x = 0;
      *((volatile char *) 0) = 0;
    }
}
