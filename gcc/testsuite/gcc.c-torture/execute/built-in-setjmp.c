int buf[20];

int
main ()
{
  char *p = (char *) alloca (20);

  strcpy (p, "test");

  if (__builtin_setjmp (buf))
    {
      if (strcmp (p, "test") != 0)
	abort ();

      exit (0);
    }

  {
    int *q = (int *) alloca (p[2] * sizeof (int));
    int i;
    
    for (i = 0; i < p[2]; i++)
      q[i] = 0;

    while (1)
      sub2 ();
  }
}

sub2 ()
{
  __builtin_longjmp (buf, 1);
}
