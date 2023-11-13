/* { dg-require-effective-target trampolines } */

void abort (void);
void exit (int);

int
g (int a, int b, int (*gi) (int, int))
{
  if ((*gi) (a, b))
    return a;
  else
    return b;
}

void
f ()
{
  int i, j;
  int f2 (int a, int b)
    {
      return a > b;
    }

  if (g (1, 2, f2) != 2)
    abort ();
}

int
main (void)
{
  f ();
  exit (0);
}
