void abort (void);
void exit (int);

int
main (void)
{
  int i, j, k[3];

  j = 0;
  for (i=0; i < 3; i++)
    {
      k[i] = j++;
    }

  for (i=2; i >= 0; i--)
    {
      if (k[i] != i)
	abort ();
    }

  exit (0);
}
