/* { dg-do compile } */
/* { dg-options "-O2 -mlong-calls" } */

extern void dump_stack (void) __attribute__ ((__cold__));
struct thread_info {
    struct task_struct *task;
};
extern struct thread_info *current_thread_info (void);

void dump_stack (void)
{
    unsigned long stack;
    show_stack ((current_thread_info ()->task), &stack);
}

void die (char *str, void *fp, int nr)
{
    dump_stack ();
    while (1);
}

