int func(int);

void
bug(int* x, int* y, unsigned long int N)
{
  unsigned long int i;
  int* t;

  while (1)
    {
      for (i=1; i<=N; i++)
	{
	  y[i] = func(x[i] - x[1]);
	  if (y[i])
	    return;
	}
      t=x; x=y; y=t;
    }
}
