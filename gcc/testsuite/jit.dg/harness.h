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
	pass ("%s: %s: %s is non-null",	     \
	      test, __func__, #PTR);	     \
      }                                      \
    else                                     \
      {                                      \
	fail ("%s: %s: %s is NULL",	     \
	      test, __func__, #PTR);	     \
	abort ();                            \
    }                                        \
  } while (0)

#define CHECK_VALUE(ACTUAL, EXPECTED) \
  do {                                       \
    if ((ACTUAL) == (EXPECTED))              \
      {                                      \
	pass ("%s: %s: actual: %s == expected: %s", \
	      test, __func__, #ACTUAL, #EXPECTED);  \
      }                                      \
    else                                     \
      {                                        \
	fail ("%s: %s: actual: %s != expected: %s", \
	      test, __func__, #ACTUAL, #EXPECTED);  \
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
	pass ("%s: %s: actual: %s == expected: %s", \
	      __func__, test, #ACTUAL, #EXPECTED);  \
      }                                      \
    else                                     \
      {                                      \
	fail ("%s: %s: actual: %s != expected: %s", \
	      __func__, test, #ACTUAL, #EXPECTED);	   \
	fprintf (stderr, "incorrect value: %f\n", actual); \
	abort ();                            \
    }                                        \
  } while (0)

#define CHECK_STRING_VALUE(ACTUAL, EXPECTED) \
  check_string_value (__func__, (ACTUAL), (EXPECTED));

#define CHECK_STRING_STARTS_WITH(ACTUAL, EXPECTED_PREFIX) \
  check_string_starts_with (__func__, (ACTUAL), (EXPECTED_PREFIX));

#define CHECK_STRING_CONTAINS(ACTUAL, EXPECTED_SUBSTRING) \
  check_string_contains (__func__, #ACTUAL, (ACTUAL), (EXPECTED_SUBSTRING));

#define CHECK(COND) \
  do {					\
    if (COND)				\
      {				\
	pass ("%s: %s: %s", test, __func__, #COND);	\
      }				\
    else				\
      {				\
	fail ("%s: %s: %s", test, __func__, #COND);	\
	abort ();			\
      }				\
  } while (0)

/* Hooks that testcases should provide.  */
extern void
create_code (gcc_jit_context *ctxt, void * user_data);

extern void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result);

extern void check_string_value (const char *funcname,
				const char *actual, const char *expected);

extern void
check_string_starts_with (const char *funcname,
			  const char *actual,
			  const char *expected_prefix);

extern void
check_string_contains (const char *funcname,
		       const char *name,
		       const char *actual,
		       const char *expected_substring);

/* Implement framework needed for turning the testcase hooks into an
   executable.  test-combination.c and test-threads.c each combine multiple
   testcases into larger testcases, so we have COMBINED_TEST as a way of
   temporarily turning off this part of harness.h.  */
#ifndef COMBINED_TEST

void check_string_value (const char *funcname,
			 const char *actual, const char *expected)
{
  if (actual && !expected)
    {
      fail ("%s: %s: actual: \"%s\" != expected: NULL",
	    funcname, test, actual);
	fprintf (stderr, "incorrect value\n");
	abort ();
    }
    if (expected && !actual)
      {
	fail ("%s: %s: actual: NULL != expected: \"%s\"",
	      funcname, test, expected);
	fprintf (stderr, "incorrect value\n");
	abort ();
      }
    if (actual && expected)
      {
	if (strcmp (actual, expected))
	  {
	    fail ("%s: %s: actual: \"%s\" != expected: \"%s\"",
		  test, funcname, actual, expected);
	    fprintf (stderr, "incorrect valuen");
	    abort ();
	  }
	pass ("%s: %s: actual: \"%s\" == expected: \"%s\"",
	      test, funcname, actual, expected);
      }
    else
      pass ("%s: actual: NULL == expected: NULL");
}

void
check_string_starts_with (const char *funcname,
			  const char *actual,
			  const char *expected_prefix)
{
  if (!actual)
    {
      fail ("%s: %s: actual: NULL != expected prefix: \"%s\"",
	    test, funcname, expected_prefix);
      fprintf (stderr, "incorrect value\n");
      abort ();
    }

  if (strncmp (actual, expected_prefix, strlen (expected_prefix)))
    {
      fail ("%s: %s: actual: \"%s\" did not begin with expected prefix: \"%s\"",
	    test, funcname, actual, expected_prefix);
      fprintf (stderr, "incorrect value\n");
      abort ();
    }

  pass ("%s: actual: \"%s\" begins with expected prefix: \"%s\"",
	test, actual, expected_prefix);
}

void
check_string_contains (const char *funcname,
		       const char *name,
		       const char *actual,
		       const char *expected_substring)
{
  if (!actual)
    {
      fail ("%s: %s, %s: actual: NULL does not contain expected substring: \"%s\"",
	    test, funcname, name, expected_substring);
      fprintf (stderr, "incorrect value\n");
      abort ();
    }

  if (!strstr (actual, expected_substring))
    {
      fail ("%s: %s: %s: actual: \"%s\" did not contain expected substring: \"%s\"",
	    test, funcname, name, actual, expected_substring);
      fprintf (stderr, "incorrect value\n");
      abort ();
    }

  pass ("%s: %s: %s: found substring: \"%s\"",
	test, funcname, name, expected_substring);
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
/* Set up logging to a logfile of the form "test-FOO.exe.log.txt".

   For example,
     SRCDIR/gcc/testsuite/jit.dg/test-hello-world.c
   is built as:
     BUILDDIR/gcc/testsuite/jit/test-hello-world.c.exe
   and is logged to
     BUILDDIR/gcc/testsuite/jit/test-hello-world.c.exe.log.txt

   The logfile must be closed by the caller.

   Note that not every testcase enables logging.  */
static FILE *
set_up_logging (gcc_jit_context *ctxt, const char *argv0)
{
  const char *logfile_name_suffix = ".log.txt";
  char *logfile_name = NULL;
  FILE *logfile = NULL;

  /* Build a logfile name of the form "test-FOO.exe.log.txt".  */
  logfile_name = (char *)malloc (strlen (argv0)
				 + strlen (logfile_name_suffix)
				 + 1);
  if (!logfile_name)
    {
      fail ("malloc failure");
      return NULL;
    }
  strcpy (logfile_name, argv0);
  strcpy (logfile_name + strlen (argv0), logfile_name_suffix);
  logfile_name[strlen (argv0) + strlen (logfile_name_suffix)] = '\0';

  logfile = fopen (logfile_name, "w");
  CHECK_NON_NULL (logfile);
  free (logfile_name);

  if (logfile)
    gcc_jit_context_set_logfile (ctxt, logfile, 0, 0);

  return logfile;
}

/* Run one iteration of the test.  */
static void
test_jit (const char *argv0, void *user_data)
{
  gcc_jit_context *ctxt;
  FILE *logfile;
  gcc_jit_result *result;

  ctxt = gcc_jit_context_acquire ();
  if (!ctxt)
    {
      fail ("gcc_jit_context_acquire failed");
      return;
    }

  logfile = set_up_logging (ctxt, argv0);

  set_options (ctxt, argv0);

  create_code (ctxt, user_data);

  /* This actually calls into GCC and runs the build, all
     in a mutex for now.  */
  result = gcc_jit_context_compile (ctxt);

  verify_code (ctxt, result);

  gcc_jit_context_release (ctxt);

  /* Once we're done with the code, this unloads the built .so file: */
  gcc_jit_result_release (result);

  if (logfile)
    fclose (logfile);
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
