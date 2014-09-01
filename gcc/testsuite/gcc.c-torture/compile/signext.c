void longprint (x)
     long long x;
{
  printf (" %d, %d\n", (unsigned) ((unsigned long long) x >> 32),
	  (unsigned) x);
}

void
k_min (p, qa, d)
     int d;
{
  int s = 1;
  long long x;

  if (s >= d)
    s -= d;

  x = ((long long)((8 * s) % 3) + qa) % d;
  longprint (x);
}

int
main ()
{
  k_min (100003, -600017, 3);
  return 0;
}
