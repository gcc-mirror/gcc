#if defined(__i386__) && defined(__FreeBSD__)
#include <ieeefp.h>
#endif

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

#if defined(__i386__) && defined(__FreeBSD__)
  /* This test case assumes extended-precision, but FreeBSD defaults to
     double-precision.  Make it so.  */
  fpsetprec (FP_PE);
#endif

  if (sizeof (double) >= 8)
    {
      k = 0x8693ba6d7d220401ULL;
      x = d (k);
      k = (unsigned long long) x;
      if (k != 0x8693ba6d7d220800ULL)
	abort ();
    }

  k = 0x8234508000000001ULL;
  x = s (k);
  k = (unsigned long long) x;
#ifdef __SPU__
  /* SPU float rounds towards zero.  */
  if (k != 0x8234500000000000ULL)
    abort ();
#else
  if (k != 0x8234510000000000ULL)
    abort ();
#endif

  exit (0);
}

