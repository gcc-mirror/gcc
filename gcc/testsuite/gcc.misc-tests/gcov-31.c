/* { dg-options "--coverage -fpath-coverage" } */
/* { dg-do compile } */
/* { dg-require-effective-target sigsetjmp } */

/* A collection of odd crashes and regressions observed when building arbitrary
   programs.  */

#include <setjmp.h>

/* Based on bash-5.2/trap.c run_pending_traps.  This revealed a case where
   adding IOR failed because there was no to replace in the phi.  */
extern void jump_to_top_level (int) __attribute__((__noreturn__));
extern sigjmp_buf return_catch;
extern int running_trap;
void
run_pending_traps ()
{
  int sig;
  if (running_trap > 0)
    jump_to_top_level (2);

  for (sig = 1; sig < (64 + 1) ; sig++)
    __sigsetjmp ((return_catch), 0);
}

/* Distilled from alsalib-1.2.11 pcm/pcm_route.c.  */
void
snd_pcm_route_convert1_many()
{
  void *top = &&fst;
  void *mid = &&snd;
  int sample = 0;
 fst:
 snd:
    goto *mid;
}
