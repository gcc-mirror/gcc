double d (unsigned long long k)
{
  double x;

  x = (double) k;
  return x;
}

float s (unsigned long long k)
{
  float x;

  x = (float) k;
  return x;
}

main ()
{
  unsigned long long int k;
  double x;

  /* CYGNUS LOCAL -- meissner/32bit doubles */
  if (sizeof (double) >= 8)
    {
      k = 0x8693ba6d7d220401ULL;
      x = d (k);
      k = (unsigned long long) x;
      if (k != 0x8693ba6d7d220800ULL)
	abort ();
    }
  /* END CYGNUS LOCAL -- meissner/32bit doubles */

  k = 0x8234508000000001ULL;
  x = s (k);
  k = (unsigned long long) x;
  if (k != 0x8234510000000000ULL)
    abort ();

  exit (0);
}

