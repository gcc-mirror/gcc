/* { dg-do compile } */
/* { dg-skip-if "" { ! { i?86-*-* x86_64-*-* } } { "*" } { "" } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct thread_info { int preempt_count; };
static inline struct thread_info *current_thread_info(void)
{
  register struct thread_info *sp asm("esp");
  return sp;
}
void testcase(void)
{
  current_thread_info()->preempt_count += 1;
}

/* We have to make sure that alias analysis treats sp as pointing
   to globals and thus the store not optimized away.  */

/* { dg-final { scan-tree-dump "->preempt_count =" "optimized" } } */
