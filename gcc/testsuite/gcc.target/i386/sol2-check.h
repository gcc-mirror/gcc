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
  ucp->uc_mcontext.gregs[EIP] += ILL_INSN_LEN;
  setcontext (ucp);
}
#endif

/* Solaris 2 before Solaris 9 4/04 cannot execute SSE/SSE2 instructions
   even if the CPU supports them.  Programs receive SIGILL instead, so
   check for that at runtime.  */
static int
sol2_check (void)
{
#if defined(__sun__) && defined(__svr4__)
  struct sigaction act, oact;

  act.sa_handler = sigill_hdlr;
  sigemptyset (&act.sa_mask);
  /* Need to set SA_SIGINFO so a ucontext_t * is passed to the handler.  */
  act.sa_flags = SA_SIGINFO;
  sigaction (SIGILL, &act, &oact);

  ILL_INSN;

  sigaction (SIGILL, &oact, NULL);

  if (sigill_caught)
    exit (0);
  else
    return 1;
#else
  return 1;
#endif /* __sun__ && __svr4__ */
}
