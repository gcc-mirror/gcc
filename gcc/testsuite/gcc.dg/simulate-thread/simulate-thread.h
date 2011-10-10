int simulate_thread_fini = 0;

void __attribute__((noinline))
simulate_thread_done ()
{
  simulate_thread_fini = 1;
}
