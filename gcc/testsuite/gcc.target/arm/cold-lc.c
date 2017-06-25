/* { dg-do compile } */
/* { dg-options "-O2 -mlong-calls" } */
/* { dg-final { scan-assembler-not "bl\[^\n\]*dump_stack" } } */

extern void dump_stack (void) __attribute__ ((__cold__)) __attribute__ ((noinline));
struct thread_info {
    struct task_struct *task;
};
extern struct thread_info *current_thread_info (void);
extern int show_stack (struct task_struct *, unsigned long *);

void dump_stack (void)
{
  unsigned long stack;
  show_stack ((current_thread_info ()->task), &stack);
}

void die (char *str, void *fp, int nr)
{
  if (nr)
    dump_stack ();
  while (1);
}

