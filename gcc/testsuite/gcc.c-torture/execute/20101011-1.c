/* With -fnon-call-exceptions 0 / 0 should not be eliminated.  The .x
   file sets the option.  */

#ifdef SIGNAL_SUPPRESS
# define DO_TEST 0
#elif defined (__powerpc__) || defined (__PPC__) || defined (__ppc__) || defined (__POWERPC__) || defined (__ppc)
  /* On PPC division by zero does not trap.  */
# define DO_TEST 0
#elif defined (__SPU__)
  /* On SPU division by zero does not trap.  */
# define DO_TEST 0
#else
# define DO_TEST 1
#endif

#if DO_TEST

#include <signal.h>

void
sigfpe (int signum __attribute__ ((unused)))
{
  exit (0);
}

#endif

/* When optimizing, the compiler is smart enough to constant fold the
   static unset variables i and j to produce 0 / 0, but it can't
   eliminate the assignment to the global k.  */
static int i;
static int j;
int k;

int
main ()
{
#ifdef DO_TEST
  signal (SIGFPE, sigfpe);
  k = i / j;
  abort ();
#else
  exit (0);
#endif
}
