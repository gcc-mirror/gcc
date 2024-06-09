/* { dg-require-effective-target trampolines } */

void abort (void);
void exit (int);

extern int foo (int, int, int (*) (int, int, int, int, int, int, int));

int z;

int
main (void)
{
  int sum = 0;
  int i;

  int nested (int a, int b, int c, int d, int e, int f, int g)
    {
      z = c + d + e + f + g;
      
      if (a > 2 * b)
        return a - b;
      else
        return b - a;
    }

  for (i = 0; i < 10; ++i)
    {
      int j;

      for (j = 0; j < 10; ++j)
        {
          int k;

          for (k = 0; k < 10; ++k)
            sum += foo (i, j > k ? j - k : k - j, nested);
        }
    }

  if (sum != 2300)
    abort ();

  if (z != 0x1b)
    abort ();

  exit (0);
}

int
foo (int a, int b, int (* fp) (int, int, int, int, int, int, int))
{
  return fp (a, b, a, b, a, b, a);
}
