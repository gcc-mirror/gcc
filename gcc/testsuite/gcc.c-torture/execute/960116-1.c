void abort (void);
void exit (int);

static inline int
p (int *p)
{
  return !((long) p & 1);
}

int
f (int *q)
{
  if (p (q) && *q)
    return 1;
  return 0;
}

int
main (void)
{
  if (f ((int*) 0xffffffff) != 0)
    abort ();
  exit (0);
}
