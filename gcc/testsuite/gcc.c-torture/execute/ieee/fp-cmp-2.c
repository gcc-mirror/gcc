#ifndef SIGNAL_SUPPRESS
#include <signal.h>
#endif

void abort (void);
void exit (int);

float fnan = 1.0f/0.0f - 1.0f/0.0f;
float x = 1.0f;

void leave ()
{
  exit (0);
}

int
main (void)
{
#if ! defined (__vax__) && ! defined (_CRAY)
  /* Move this line earlier, for architectures (like alpha) that issue 
     SIGFPE on the first comparisons. */
#ifndef SIGNAL_SUPPRESS
  /* Some machines catches a SIGFPE when a NaN is compared.
     Let this test succeed o such machines.  */
  signal (SIGFPE, leave);
#endif
  /* NaN is an IEEE unordered operand.  All these test should be false.  */
  if (fnan == fnan)
    abort ();
  if (fnan != x)
    x = 1.0;
  else
    abort ();

  if (fnan < x)
    abort ();
  if (fnan > x)
    abort ();
  if (fnan <= x)
    abort ();
  if (fnan >= x)
    abort ();
  if (fnan == x)
    abort ();
#endif
  exit (0);
}
