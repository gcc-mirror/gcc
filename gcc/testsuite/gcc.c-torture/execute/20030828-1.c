void abort (void);
void exit (int);

const int *p;

int bar (void)
{
  return *p + 1;
}

int
main (void)
{
  /* Variable 'i' is never used but it's aliased to a global pointer.  The
     alias analyzer was not considering that 'i' may be used in the call to
     bar().  */
  const int i = 5;
  p = &i;
  if (bar() != 6)
    abort ();
  exit (0);
}
