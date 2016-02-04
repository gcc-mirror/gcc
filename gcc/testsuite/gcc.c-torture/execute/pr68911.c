extern void abort (void);

char a;
int b, c;
short d;

int main ()
{
  unsigned e = 2;
  unsigned timeout = 0;

  for (; c < 2; c++)
    {
      int f = ~e / 7;
      if (f)
	a = e = ~(b && d);
      while (e < 94)
	{
	  e++;
	  if (++timeout > 100)
	    goto die;
	}
    }
  return 0;
die:
  abort ();
}
