int
main (void)
{
  long long   x;
  int         n;

  n = 9;
  x = (((long long) n) << 55) / 0xff; 

  if (x == 0)
    abort ();

  x = (((long long) 9) << 55) / 0xff;

  if (x == 0)
    abort ();

  exit (0);
}
