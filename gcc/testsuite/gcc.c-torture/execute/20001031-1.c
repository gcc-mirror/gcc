extern void abort (void);
extern void exit (int);

void t1 (int x)
{
  if (x != 4100)
    abort ();
}

int t2 (void)
{
  int i;
  t1 ((i = 4096) + 4);
  return i;
}

void t3 (long long x)
{
  if (x != 0x80000fffULL)
    abort ();
}

long long t4 (void)
{
  long long i;
  t3 ((i = 4096) + 0x7fffffffULL);
  return i;
}

main ()
{
  if (t2 () != 4096)
    abort ();
  if (t4 () != 4096)
    abort ();
  exit (0);
}
