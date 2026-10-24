/* Unwinding out of a signal handler that runs on an alternate stack must
   copy the handler's register backing store back to the thread's own,
   and nothing more.  In a thread, the memory stack grows down towards the
   top of the register backing store, so writing past the end of the copy
   corrupts the memory stack.  */
/* { dg-do run { target *-*-linux* } } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-O1 -pthread" } */

#include <limits.h>
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include <unwind.h>

static _Unwind_Reason_Code
trace (struct _Unwind_Context *ctx, void *arg)
{
  return _URC_NO_REASON;
}

static void
handler (int sig)
{
  _Unwind_Backtrace (trace, 0);
}

/* Fill the memory stack down to 1 KB above the register stack, raise the
   signal, and check that the filled memory survived.  raise needs some
   of the gap, but a larger one can hide copying too much.  */
static int __attribute__ ((noinline))
leaf (void)
{
  char here;
  size_t size = &here - (char *) __builtin_ia64_bsp () - 1024;
  volatile unsigned char *buf = __builtin_alloca (size);
  size_t i;

  memset ((void *) buf, 0x5a, size);
  raise (SIGUSR1);
  for (i = 0; i < size; i++)
    if (buf[i] != 0x5a)
      return 0;
  return 1;
}

/* Recurse N times before raising the signal, and check that the frames
   are intact afterwards.  */
static long __attribute__ ((noinline))
deep (int n)
{
  volatile long a = n, b = n * 2, c = n * 3, d = n * 4;
  long r;

  if (n == 0)
    return leaf () ? 0 : -1;
  r = deep (n - 1);
  if (r < 0)
    return r;
  return r + a + b + c + d;
}

static stack_t alt;

static void *
thread_fn (void *arg)
{
  int depth = *(int *) arg;
  long want = 0;
  int n;

  for (n = 1; n <= depth; n++)
    want += 10L * n;
  if (sigaltstack (&alt, 0) != 0)
    abort ();
  if (deep (depth) != want)
    abort ();
  return 0;
}

int
main (void)
{
  /* The depths move the thread's backing store relative to its NaT
     collections, so that the copy ends in either of the two runs that
     ia64_copy_rbs copies between them.  */
  static int depths[] = { 0, 5, 10, 20, 40, 80 };
  size_t page = sysconf (_SC_PAGESIZE);
  size_t stack_size = (PTHREAD_STACK_MIN + page - 1) & ~(page - 1);
  size_t alt_size = 1 << 20;
  char *map;
  struct sigaction sa;
  pthread_attr_t attr;
  pthread_t t;
  unsigned i;

  map = mmap (0, stack_size + alt_size, PROT_READ | PROT_WRITE,
	      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (map == MAP_FAILED)
    abort ();
  alt.ss_sp = map + stack_size;
  alt.ss_size = alt_size;

  memset (&sa, 0, sizeof sa);
  sa.sa_handler = handler;
  sa.sa_flags = SA_ONSTACK;
  if (sigaction (SIGUSR1, &sa, 0) != 0)
    abort ();

  if (pthread_attr_init (&attr) != 0
      || pthread_attr_setstack (&attr, map, stack_size) != 0)
    abort ();
  for (i = 0; i < sizeof depths / sizeof depths[0]; i++)
    {
      if (pthread_create (&t, &attr, thread_fn, &depths[i]) != 0)
	abort ();
      pthread_join (t, 0);
    }
  return 0;
}
