#if defined(__sun__) && defined(__svr4__)
/* Make sure sigaction() is declared even with -std=c99.  */
#define __EXTENSIONS__
#include <signal.h>
#include <ucontext.h>

static volatile sig_atomic_t sigill_caught;

static void
sigill_hdlr (int sig __attribute((unused)),
	     siginfo_t *sip __attribute__((unused)),
	     ucontext_t *ucp)
{
  sigill_caught = 1;
  /* Set PC to the instruction after the faulting one to skip over it,
     otherwise we enter an infinite loop.  */
  ucp->uc_mcontext.gregs[EIP] += 4;
  setcontext (ucp);
}
#endif

/* Check if the OS supports executing SSE instructions.  This function is
   only used in sse-check.h, sse2-check.h, and sse3-check.h so far since
   Solaris 8 and 9 won't run on newer CPUs anyway.  */

static int
sse_os_support (void)
{
#if defined(__sun__) && defined(__svr4__)
  /* Solaris 2 before Solaris 9 4/04 cannot execute SSE instructions
     even if the CPU supports them.  Programs receive SIGILL instead, so
     check for that at runtime.  */

  struct sigaction act, oact;

  act.sa_handler = sigill_hdlr;
  sigemptyset (&act.sa_mask);
  /* Need to set SA_SIGINFO so a ucontext_t * is passed to the handler.  */
  act.sa_flags = SA_SIGINFO;
  sigaction (SIGILL, &act, &oact);

  /* We need a single SSE instruction here so the handler can safely skip
     over it.  */
  __asm__ volatile ("movss %xmm2,%xmm1");

  sigaction (SIGILL, &oact, NULL);

  if (sigill_caught)
    exit (0);
  else
    return 1;
#else
  return 1;
#endif /* __sun__ && __svr4__ */
}
