long long
xlrandom ()
{
  long long x;
  unsigned a;
  int bits = 64;
  unsigned b;

  do
    {
      a = random ();
      b = (a & 15) + 1;
      x <<= b;				/* shift up 1-16 steps */
      a = (a >> 18) & 1;
      if (a)
	x |= (unsigned) (1 << b) - 1;
      bits -= b;
    }
  while (bits >= 0);
  return x;
}


unsigned long long __udivmoddi4();

main ()
{
  int i;
  unsigned long long n, d, q, r, rr;

  for (i = 0; ;i++)
    {
      n = xlrandom ();
      d = xlrandom ();
      if (d == 0)
	continue;

      q = __udivmoddi4 (n, d, &r);

      if (i % 1000000 == 0)
	printf ("Testing udivmoddi4: %d iterations made\n", i);

      rr = n - q * d;
      if (rr != r || r >= d)
	{
	  printf ("Testing udivmoddi4: failure after %d iterations\n", i);
	  printf ("n=%lX%08lX\n", (unsigned) (n >> 32), (unsigned) n);
	  printf ("d=%lX%08lX\n", (unsigned) (d >> 32), (unsigned) d);
	  printf ("q=%lX%08lX\n", (unsigned) (q >> 32), (unsigned) q);
	  printf ("r=%lX%08lX\n", (unsigned) (r >> 32), (unsigned) r);
	  printf ("rr=%lX%08lX\n", (unsigned) (rr >> 32), (unsigned) rr);
	  abort ();
	}
    }

}
