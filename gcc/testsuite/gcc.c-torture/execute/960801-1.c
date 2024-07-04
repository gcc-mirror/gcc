void abort (void);
void exit (int);

unsigned
f ()
{
  long long l2;
  unsigned short us;
  unsigned long long ul;
  short s2;

  ul = us = l2 = s2 = -1;
  return ul;
}

unsigned long long
g ()
{
  long long l2;
  unsigned short us;
  unsigned long long ul;
  short s2;

  ul = us = l2 = s2 = -1;
  return ul;
}

int
main (void)
{
  if (f () != (unsigned short) -1)
    abort ();
  if (g () != (unsigned short) -1)
    abort ();
  exit (0);
}
