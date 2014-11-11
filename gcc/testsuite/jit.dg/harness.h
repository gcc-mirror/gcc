/*
  Code shared between multiple testcases.

  This file contains "main" and support code.
  Each testcase should implement the following hooks:

    extern void
    create_code (gcc_jit_context *ctxt, void * user_data);

    extern void
    verify_code (gcc_jit_context *ctxt, gcc_jit_result *result);

 */
#include <stdlib.h>
#include <stdio.h>

/* test-threads.c use threads, but dejagnu.h isn't thread-safe; there's a
   shared "buffer", and the counts of passed/failed etc are globals.

   The solution is to use macros to rename "pass" and "fail", replacing them
   with mutex-guarded alternatives.  */
#ifdef MAKE_DEJAGNU_H_THREADSAFE
#define pass dejagnu_pass
#define fail dejagnu_fail
#define note dejagnu_note
#endif

#include <dejagnu.h>

#ifdef MAKE_DEJAGNU_H_THREADSAFE
#undef pass
#undef fail
#undef note
#endif

static char test[1024];

#define CHECK_NON_NULL(PTR) \
  do {                                       \
    if ((PTR) != NULL)                       \
      {                                      \
	pass ("%s: %s is non-null", test, #PTR); \
      }                                      \
    else                                     \
      {                                      \
	fail ("%s: %s is NULL", test, #PTR); \
	abort ();                            \
    }                                        \
  } while (0)

#define CHECK_VALUE(ACTUAL, EXPECTED) \
  do {                                       \
    if ((ACTUAL) == (EXPECTED))              \
      {                                      \
	pass ("%s: actual: %s == expected: %s", test, #ACTUAL, #EXPECTED); \
      }                                      \
    else                                     \
      {                                        \
	fail ("%s: actual: %s != expected: %s", test, #ACTUAL, #EXPECTED); \
	fprintf (stderr, "incorrect value\n"); \
	abort ();                              \
    }                                        \
  } while (0)

#define CHECK_DOUBLE_VALUE(ACTUAL, EXPECTED) \
  do {                                       \
    double expected = (EXPECTED);	     \
    double actual = (ACTUAL);		     \
    if (abs (actual - expected) < 0.00001)   \
      {                                      \
	pass ("%s: actual: %s == expected: %s", test, #ACTUAL, #EXPECTED); \
      }                                      \
    else                                     \
      {                                      \
	fail ("%s: actual: %s != expected: %s", test, #ACTUAL, #EXPECTED); \
	fprintf (stderr, "incorrect value: %f\n", actual); \
	abort ();                            \
    }                                        \
  } while (0)

#define CHECK_STRING_VALUE(ACTUAL, EXPECTED) \
  check_string_value ((ACTUAL), (EXPECTED));

#define CHECK(COND) \
  do {					\
    if (COND)				\
      {				\
	pass ("%s: %s", test, #COND);	\
      }				\
    else				\
      {				\
	fail ("%s: %s", test, #COND);	\
	abort ();			\
      }				\
  } while (0)

/* Hooks that testcases should provide.  */
extern void
create_code (gcc_jit_context *ctxt, void * user_data);

extern void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result);

extern void check_string_value (const char *actual, const char *expected);

/* Implement framework needed for turning the testcase hooks into an
   executable.  test-combination.c and test-threads.c each combine multiple
   testcases into larger testcases, so we have COMBINED_TEST as a way of
   temporarily turning off this part of harness.h.  */
#ifndef COMBINED_TEST

void check_string_value (const char *actual, const char *expected)
{
  if (actual && !expected)
    {
      fail ("%s: actual: \"%s\" != expected: NULL", test, actual);
	fprintf (stderr, "incorrect value\n");
	abort ();
    }
    if (expected && !actual)
      {
	fail ("%s: actual: NULL != expected: \"%s\"", test, expected);
	fprintf (stderr, "incorrect value\n");
	abort ();
      }
    if (actual && expected)
      {
	if (strcmp (actual, expected))
	  {
	    fail ("%s: actual: \"%s\" != expected: \"%s\"", test, actual, expected);
	    fprintf (stderr, "incorrect valuen");
	    abort ();
	  }
	pass ("%s: actual: \"%s\" == expected: \"%s\"", test, actual, expected);
      }
    else
      pass ("%s: actual: NULL == expected: NULL");
}

static void set_options (gcc_jit_context *ctxt, const char *argv0)
{
  /* Set up options.  */
  gcc_jit_context_set_str_option (
    ctxt,
    GCC_JIT_STR_OPTION_PROGNAME,
    argv0);
  gcc_jit_context_set_int_option (
    ctxt,
    GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL,
    3);
  gcc_jit_context_set_bool_option (
    ctxt,
    GCC_JIT_BOOL_OPTION_DEBUGINFO,
    1);
  gcc_jit_context_set_bool_option (
    ctxt,
    GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE,
    0);
  gcc_jit_context_set_bool_option (
    ctxt,
    GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE,
    0);
  gcc_jit_context_set_bool_option (
    ctxt,
    GCC_JIT_BOOL_OPTION_SELFCHECK_GC,
    1);
  gcc_jit_context_set_bool_option (
    ctxt,
    GCC_JIT_BOOL_OPTION_DUMP_SUMMARY,
    0);
}

#ifndef TEST_ESCHEWS_TEST_JIT
/* Run one iteration of the test.  */
static void
test_jit (const char *argv0, void *user_data)
{
  gcc_jit_context *ctxt;
  gcc_jit_result *result;

  ctxt = gcc_jit_context_acquire ();
     /* FIXME: error-handling */

  set_options (ctxt, argv0);

  create_code (ctxt, user_data);

  /* This actually calls into GCC and runs the build, all
     in a mutex for now.  */
  result = gcc_jit_context_compile (ctxt);

  verify_code (ctxt, result);

  gcc_jit_context_release (ctxt);

  /* Once we're done with the code, this unloads the built .so file: */
  gcc_jit_result_release (result);
}
#endif /* #ifndef TEST_ESCHEWS_TEST_JIT */

/* We want to prefix all unit test results with the test, but dejagnu.exp's
   host_execute appears to get confused by the leading "./" of argv0,
   leading to all tests simply reporting as a single period character ".".

   Hence strip out the final component of the path to the program name,
   so that we can use that in unittest reports.  */
const char*
extract_progname (const char *argv0)
{
  const char *p;

  p = argv0 + strlen (argv0);
  while (p != argv0 && p[-1] != '/')
    --p;
  return p;
}

#ifndef TEST_PROVIDES_MAIN
int
main (int argc, char **argv)
{
  int i;

  for (i = 1; i <= 5; i++)
    {
      snprintf (test, sizeof (test),
		"%s iteration %d of %d",
                extract_progname (argv[0]),
                i, 5);

      //printf ("ITERATION %d\n", i);
      test_jit (argv[0], NULL);
      //printf ("\n");
    }

  totals ();

  return 0;
}
#endif /* #ifndef TEST_PROVIDES_MAIN */

#endif /* #ifndef COMBINED_TEST */
