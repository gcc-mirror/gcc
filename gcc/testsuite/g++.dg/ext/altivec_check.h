/* A runtime check for AltiVec capability.  */
/* Contributed by Ziemowit Laski  <zlaski@apple.com>  */

#include <signal.h>
extern "C" void exit(int);

void 
sig_ill_handler (int sig)
{
    exit (0);
}

void altivec_check(void) {

  /* Exit on systems without AltiVec.  */
  signal (SIGILL, sig_ill_handler);
#ifdef __MACH__
  asm volatile ("vor v0,v0,v0");
#else
  asm volatile ("vor 0,0,0");
#endif
  signal (SIGILL, SIG_DFL);
}
