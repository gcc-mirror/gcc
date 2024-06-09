void abort (void);
void exit (int);

int f (int i) { return 12; }
int g () { return 0; }

int
main (void)
{
  int i, s;

  for (i = 0; i < 32; i++)
    {
      s = f (i);

      if (i == g ())
	s = 42;
      if (i == 0 || s == 12)
	;
      else
	abort ();
    }

  exit (0);
}
