/* This used to fail on h8300.  */

extern void abort (void);
extern void exit (int);

unsigned long
foo (unsigned long n)
{
  return (~n >> 3) & 1;
}

int
main ()
{
  if (foo (1 << 3) != 0)
    abort ();

  if (foo (0) != 1)
    abort ();

  exit (0);
}
