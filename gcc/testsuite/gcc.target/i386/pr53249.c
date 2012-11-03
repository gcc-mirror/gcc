/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -mx32 -ftls-model=initial-exec -maddress-mode=short" } */

struct gomp_task
{
  struct gomp_task *parent;
};

struct gomp_thread
{
  int foo1;
  struct gomp_task *task;
};

extern __thread struct gomp_thread gomp_tls_data;

void
__attribute__ ((noinline))
gomp_end_task (void)
{
  struct gomp_thread *thr = &gomp_tls_data;
  struct gomp_task *task = thr->task;

  thr->task = task->parent;
}
