/* { dg-do run } */
/* { dg-require-effective-target split_stack } */
/* { dg-require-effective-target pthread_h } */
/* { dg-require-effective-target ucontext_h } */
/* { dg-options "-pthread -fsplit-stack" } */

#include <stdlib.h>
#include <pthread.h>
#include <ucontext.h>

extern void __splitstack_getcontext (void *context[10]);

extern void __splitstack_setcontext (void *context[10]);

extern void *__splitstack_makecontext (size_t, void *context[10], size_t *);

extern void __splitstack_block_signals (int *, int *);

extern void __splitstack_block_signals_context (void *context[10], int *,
						int *);

extern void *__splitstack_find (void *, void *, size_t *, void **, void **,
				void **);

extern void *__splitstack_find_context (void *context[10], size_t *, void **,
					void **, void **);

static ucontext_t c1;
static void *s1[10];

static ucontext_t c2;
static void *s2[10];

static void swap (ucontext_t *, void *fs[10], ucontext_t *, void *ts[10])
  __attribute__ ((no_split_stack));

static void
swap (ucontext_t *fu, void *fs[10], ucontext_t *tu, void *ts[10])
{
  __splitstack_getcontext (fs);
  __splitstack_setcontext (ts);
  swapcontext (fu, tu);
  __splitstack_setcontext (fs);
}

/* Use a noinline function to ensure that the buffer is not removed
   from the stack.  */
static void use_buffer (char *buf) __attribute__ ((noinline));
static void
use_buffer (char *buf)
{
  buf[0] = '\0';
}

static void
down (int i, const char *msg, ucontext_t *me, void *mes[10],
      ucontext_t *other, void *others[10])
{
  char buf[10000];

  if (i > 0)
    {
      use_buffer (buf);
      swap (me, mes, other, others);
      down (i - 1, msg, me, mes, other, others);
    }
  else
    {
      int c = 0;
      void *stack;
      size_t stack_size;
      void *next_segment = NULL;
      void *next_sp = NULL;
      void *initial_sp = NULL;

      stack = __splitstack_find_context (mes, &stack_size, &next_segment,
					&next_sp, &initial_sp);
      if (stack != NULL)
	{
	  ++c;
	  while (__splitstack_find (next_segment, next_sp, &stack_size,
				    &next_segment, &next_sp, &initial_sp)
		 != NULL)
	    ++c;
	}
    }
}

static void
go1 (void)
{
  down (1000, "go1", &c1, s1, &c2, s2);
  pthread_exit (NULL);
}

static void
go2 (void)
{
  down (1000, "go2", &c2, s2, &c1, s1);
  pthread_exit (NULL);
}

struct thread_context
{
  ucontext_t *u;
  void **s;
};

static void *start_thread (void *) __attribute__ ((no_split_stack));

static void *
start_thread (void *context)
{
  struct thread_context *tc = (struct thread_context *) context;
  int block;

  block = 0;
  __splitstack_block_signals (&block, NULL);
  __splitstack_setcontext (tc->s);
  setcontext (tc->u);
  abort ();
}

int
main (int argc __attribute__ ((unused)), char **argv __attribute__ ((unused)))
{
  pthread_t tid;
  int err;
  size_t size;
  struct thread_context tc;
  int block;

  if (getcontext (&c1) < 0)
    abort ();

  c2 = c1;

  c1.uc_stack.ss_sp = __splitstack_makecontext (8192, &s1[0], &size);
  if (c1.uc_stack.ss_sp == NULL)
    abort ();
  c1.uc_stack.ss_flags = 0;
  c1.uc_stack.ss_size = size;
  c1.uc_link = NULL;
  block = 0;
  __splitstack_block_signals_context (&s1[0], &block, NULL);
  makecontext (&c1, go1, 0);

  c2.uc_stack.ss_sp = __splitstack_makecontext (8192, &s2[0], &size);
  if (c2.uc_stack.ss_sp == NULL)
    abort ();
  c2.uc_stack.ss_flags = 0;
  c2.uc_stack.ss_size = size;
  c2.uc_link = NULL;
  __splitstack_block_signals_context (&s2[0], &block, NULL);
  makecontext (&c2, go2, 0);

  block = 0;
  __splitstack_block_signals (&block, NULL);

  tc.u = &c1;
  tc.s = &s1[0];
  err = pthread_create (&tid, NULL, start_thread, &tc);
  if (err != 0)
    abort ();

  err = pthread_join (tid, NULL);
  if (err != 0)
    abort ();

  return 0;
}
