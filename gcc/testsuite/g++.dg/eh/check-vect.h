/* Check if system supports SIMD.  Copied from gcc.dg/vect/tree-vect.h.  */
#include <signal.h>

extern "C" void abort (void);
extern "C" void exit (int);

void
sig_ill_handler (int sig)
{
  exit(0);
}

void check_vect (void)
{
  signal(SIGILL, sig_ill_handler);
#if defined(__ppc__) || defined(__ppc64__) || defined(__powerpc__) || defined(powerpc)
  /* Altivec instruction, 'vor %v0,%v0,%v0'.  */
  asm volatile (".long 0x10000484");
#elif defined(__i386__) || defined(__x86_64__)
  /* SSE2 instruction: movsd %xmm0,%xmm0 */
  asm volatile (".byte 0xf2,0x0f,0x10,0xc0");
#elif defined(__sparc__)
  asm volatile (".word\t0x81b007c0");
#endif
  signal (SIGILL, SIG_DFL);
}
