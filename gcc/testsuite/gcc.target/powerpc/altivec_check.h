/* A runtime check for AltiVec capability.  */
/* Contributed by Ziemowit Laski  <zlaski@apple.com>  */

#include <signal.h>
extern void exit (int);
extern void abort (void);

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

void altivec_cell_check (void)
{
#ifdef __PPU__
  /* Exit on systems without the Cell Altivec instructions.  */
  signal (SIGILL, sig_ill_handler);
#ifdef __MACH__
  asm volatile ("vor v0,v0,v0");
  asm volatile ("lvlx v0,r0,r0");
#else
  asm volatile ("vor 0,0,0");
  asm volatile ("lvlx 0,0,0");
#endif
  signal (SIGILL, SIG_DFL);
#else
  /* altivec_cell_check shouldn't be called without -mcpu=cell.  */
  abort ();
#endif
}
