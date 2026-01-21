/* Reduced from a case in haproxy's haproxy.c where -fanalyzer
   erroneously considered the __atomic_feth_add (&warn_fail, 1, 0) to
   affect "ti" rather than "warn_fail".  */

struct thread_info
{
  unsigned tid;
};
extern struct thread_info ha_thread_info[64];
extern __thread const struct thread_info *ti;
extern __thread unsigned int tid;

static inline void
ha_set_thread (const struct thread_info *thr)
{
  if (thr)
    tid = thr->tid;
  else
    {
      tid = 0;
      ti = &ha_thread_info[0];
    }
}

void
run_thread_poll_loop (const struct thread_info *thr)
{
  static int warn_fail;

  ha_set_thread (thr);

  __atomic_fetch_add (&warn_fail, 1, 0);
}
