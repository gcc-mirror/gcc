void abort (void);
void exit (int);

int sum;

void
g (int i)
{
  sum += i;
}

void
f(int j)
{
  int i;

  for (i = 0; i < 9; i++)
    {
      j++;
      g (j);
      j = 9;
    }
}

int
main ()
{
  f (0);
  if (sum != 81)
    abort ();
  exit (0);
}

