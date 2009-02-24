extern void abort (void);

__attribute__((noinline)) void
foo (void *p)
{
  long l = (long) p;
  if (l < 0 || l > 6)
    abort ();
}

int
main ()
{
  short i;
  for (i = 6; i >= 0; i--)
    foo ((void *) (long) i);
  return 0;
}
