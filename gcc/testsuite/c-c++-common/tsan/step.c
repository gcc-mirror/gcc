/* { dg-do compile } */

extern int sched_yield (void);
static volatile int serial = 0;

__attribute__((no_sanitize_thread))
void step (int i)
{
   while (__atomic_load_n (&serial, __ATOMIC_ACQUIRE) != i - 1)
     sched_yield ();
   __atomic_store_n (&serial, i, __ATOMIC_RELEASE);
}

/* { dg-final { scan-assembler-not "__tsan_func_entry" } } */
/* { dg-final { scan-assembler-not "__tsan_func_exit" } } */
