/* { dg-do run } */
/* { dg-options "" } */

/* FIXME: this test needs to be conditional to systems with POSIX signals.  */

#include <signal.h>

extern void exit(int status);
extern void abort(void);

void go_quietly (int arg)
{
  exit (0);
}

int main()
{
  _Decimal32 d = 1.5df;

  /* Enable signaling NaNs using a scaffolding libgcc function.  */
  __dfp_enable_traps ();
  signal (SIGFPE, go_quietly);

  d = d / 0.0df;

  /* Never reached.  */
  abort ();
  return 0;
}
