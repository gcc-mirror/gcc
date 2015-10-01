/* { dg-do compile } */
/* { dg-require-effective-target arm_arm_ok } */ 
/* { dg-require-effective-target arm_arch_v8a_ok } */
/* { dg-options "-O2 -marm" } */
/* { dg-add-options arm_arch_v8a } */

struct backtrace_state
{
  int threaded;
  int lock_alloc;
};

void foo (struct backtrace_state *state)
{
  if (state->threaded)
    __sync_lock_release (&state->lock_alloc);
}

/* { dg-final { scan-assembler "stlne" } } */
