void abort (void);
void exit (int);

struct tiny
{
  char c;
};

void
f (int n, struct tiny x, struct tiny y, struct tiny z, long l)
{
  if (x.c != 10)
    abort();

  if (y.c != 11)
    abort();

  if (z.c != 12)
    abort();

  if (l != 123)
    abort ();
}

int
main (void)
{
  struct tiny x[3];
  x[0].c = 10;
  x[1].c = 11;
  x[2].c = 12;
  f (3, x[0], x[1], x[2], (long) 123);
  exit(0);
}

