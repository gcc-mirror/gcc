/* Test that exact underflow in __float128 signals the underflow
   exception if trapping is enabled, but does not raise the flag
   otherwise.  */

/* { dg-do run { target i?86-*-*gnu* x86_64-*-*gnu* ia64-*-*gnu* } } */
/* { dg-options "-D_GNU_SOURCE" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>
#include <setjmp.h>
#include <signal.h>
#include <stdlib.h>

volatile sig_atomic_t caught_sigfpe;
sigjmp_buf buf;

static void
handle_sigfpe (int sig)
{
  caught_sigfpe = 1;
  siglongjmp (buf, 1);
}

int
main (void)
{
  volatile __float128 a = 0x1p-16382q, b = 0x1p-2q;
  volatile __float128 r;
  r = a * b;
  if (fetestexcept (FE_UNDERFLOW))
    abort ();
  if (r != 0x1p-16384q)
    abort ();
  feenableexcept (FE_UNDERFLOW);
  signal (SIGFPE, handle_sigfpe);
  if (sigsetjmp (buf, 1) == 0)
    r = a * b;
  if (!caught_sigfpe)
    abort ();
  exit (0);
}
