extern int f1 (int, void *);
extern int *f2 (void) __attribute__ ((__const__));
extern int f3 (int, void *);

int
test (int x, char *y, int z)
{
  int b = 0;

  if (x < 1024)
    {
      y[0] = '\0';

      do
	{
	  switch (f1 (x, y + b))
	    {
	    case -1:
	      if (b == 0)
		return -1;
	      else
		return b;

	    default:
	      b++;
	    }
	}
      while (y[b - 1] != '\0' && y[b - 1] != '\n' && b < z);
    }
  else
    {
      do
	{
	  switch (f3 (x, y + b))
	    {
	    case -1:
	      if ((*f2 ()) == 4)
		continue;
	      if (b == 0)
		return -1;
	      else
		return b;

	    default:
	      b++;
	    }
	}
      while (y[b - 1] != '\0' && y[b - 1] != '\n' && b < z);
    }
  return b;
}
