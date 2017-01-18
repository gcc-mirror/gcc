/* test-threads.c

   As per test-combination.c, construct a test case by combining other test
   cases, to try to shake out state issues.  However each test runs in a
   separate thread.  */

#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>

/* dejagnu.h isn't thread-safe; there's a shared "buffer", and the counts
   of "passed"/"failed" etc are globals.

   We get around this by putting a mutex around pass/fail calls.
 */

static pthread_mutex_t dg_mutex = PTHREAD_MUTEX_INITIALIZER;

/* By defining MAKE_DEJAGNU_H_THREADSAFE before we include harness.h,
   harness.h injects macros before including <dejagnu.h> so that the
   pass/fail functions become "dejagnu_pass"/"dejagnu_fail" etc.  */

/* Forward decls of our implementations of pass/fail/note.  */

inline void
pass (const char* fmt, ...);

inline void
fail (const char* fmt, ...);

inline void
note (const char* fmt, ...);

#define MAKE_DEJAGNU_H_THREADSAFE

/* We also need to provide our own version of TEST_NAME.  */
#define TEST_NAME

/* We can now include all of the relevant selftests.  */

#include "all-non-failing-tests.h"

#define TEST_PROVIDES_MAIN
#define TEST_ESCHEWS_TEST_JIT

/* Now construct a test case from all the other test cases.

   We undefine COMBINED_TEST so that we can now include harness.h
   "for real".  */
#undef COMBINED_TEST
#include "harness.h"

/* We now provide our own implementations of "pass"/"fail"/"note", which
   call the underlying dejagnu implementations, but with a mutex.  */

inline void
pass (const char* fmt, ...)
{
  va_list ap;
  char buffer[512];

  va_start (ap, fmt);
  vsnprintf (buffer, sizeof (buffer), fmt, ap);
  va_end (ap);

  pthread_mutex_lock (&dg_mutex);
  dejagnu_pass (buffer);
  pthread_mutex_unlock (&dg_mutex);
}

inline void
fail (const char* fmt, ...)
{
  va_list ap;
  char buffer[512];

  va_start (ap, fmt);
  vsnprintf (buffer, sizeof (buffer), fmt, ap);
  va_end (ap);

  pthread_mutex_lock (&dg_mutex);
  dejagnu_fail (buffer);
  pthread_mutex_unlock (&dg_mutex);
}

inline void
note (const char* fmt, ...)
{
  va_list ap;
  char buffer[512];

  va_start (ap, fmt);
  vsnprintf (buffer, sizeof (buffer), fmt, ap);
  va_end (ap);

  pthread_mutex_lock (&dg_mutex);
  dejagnu_note (buffer);
  pthread_mutex_unlock (&dg_mutex);
}

struct thread_data
{
  pthread_t m_tid;
  const struct testcase *m_testcase;
};

static const char *argv0;

void *
run_threaded_test (void *data)
{
  struct thread_data *thread = (struct thread_data *)data;
  int i;

  for (i = 0; i < 5; i++)
    {
      gcc_jit_context *ctxt;
      gcc_jit_result *result;

      note ("run_threaded_test: %s iteration: %d",
	    thread->m_testcase->m_name, i);

      ctxt = gcc_jit_context_acquire ();

      set_options (ctxt, argv0);

      thread->m_testcase->m_hook_to_create_code (ctxt, NULL);

      result = gcc_jit_context_compile (ctxt);

      thread->m_testcase->m_hook_to_verify_code (ctxt, result);

      gcc_jit_context_release (ctxt);

      /* Once we're done with the code, this unloads the built .so file: */
      gcc_jit_result_release (result);
    }

  return NULL;
}

int
main (int argc, char **argv)
{
  int i;

  snprintf (test, sizeof (test),
	    "%s",
	    extract_progname (argv[0]));

  argv0 = argv[0];

  /* The individual testcases are not thread-safe (some have their own
     global variables), so we have one thread per test-case.  */
  struct thread_data *threads =
    calloc (num_testcases, sizeof (struct thread_data));

  /* Start a thread per test-case.  */
  for (i = 0; i < num_testcases; i++)
    {
      struct thread_data *thread = &threads[i];
      thread->m_testcase = &testcases[i];
      pthread_create (&thread->m_tid,
		      NULL,
		      run_threaded_test,
		      thread);
    }

  /* Wait for all the threads to be done.  */
  for (i = 0; i < num_testcases; i++)
    {
      struct thread_data *thread = &threads[i];
      (void)pthread_join (thread->m_tid, NULL);
    }

  totals ();

  return 0;
}
