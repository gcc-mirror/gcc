/* { dg-do run { target powerpc*-*-* } } */
/* { dg-xfail-if "" { "powerpc-*-eabispe*" "powerpc-ibm-aix*" } { "-mcpu=G5" } { "" } } */
/* { dg-options "-mcpu=G5" } */

#include <signal.h>
#include <stdlib.h>

void
sig_ill_handler (int sig)
{
    exit(0);
}


int  msw(long long in)
{
  union {
    long long ll;
    int  i[2];
  } ud;
  ud.ll = in;
  return ud.i[0];
}

int main()
{

  /* Exit on systems without 64bit instructions.  */
  signal (SIGILL, sig_ill_handler);
#ifdef __MACH__
  asm volatile ("extsw r0,r0");
#else
  asm volatile ("extsw 0,0");
#endif
  signal (SIGILL, SIG_DFL);

  if (msw(1) != 0)
    abort();
  exit(0);
}
