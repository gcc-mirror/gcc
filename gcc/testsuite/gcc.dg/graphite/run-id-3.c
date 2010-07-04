extern void abort (void);

__attribute__ ((noinline)) int
foo (int *zzz, unsigned int kk)
{
  int a, b, d;

  a = b = 0;
  for (d = 0; d < 1000; d++)
    {
      if (kk != 0)
        b = *zzz;
    }

  return b;
}

int
main (void)
{
  if (foo (0, 0) != 0)
    abort();
  return 0;
}
