/*
  Code shared between multiple testcases.

  This file contains "main" and support code.
  Each testcase should implement the following hooks:

    extern void
    create_code (gcc_jit_context *ctxt, void * user_data);

  and, #ifndef TEST_COMPILING_TO_FILE,

    extern void
    verify_code (gcc_jit_context *ctxt, gcc_jit_result *result);
 */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

/* test-threads.c use threads, but dejagnu.h isn't thread-safe; there's a
   shared "buffer", and the counts of passed/failed etc are globals.

   The solution is to use macros to rename "pass" and "fail", replacing them
   with mutex-guarded alternatives.  */
#ifdef MAKE_DEJAGNU_H_THREADSAFE
#define pass dejagnu_pass
#define fail dejagnu_fail
#define note dejagnu_note
#endif

#include "jit-dejagnu.h"

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

#define CHECK_VECTOR_VALUE(LEN, ACTUAL, EXPECTED) \
  do {                                       \
    for (int __check_vector_it = 0; __check_vector_it < LEN; ++__check_vector_it) { \
      if ((ACTUAL)[__check_vector_it] != (EXPECTED)[__check_vector_it]) { \
          fail ("%s: %s: actual: %s != expected: %s (position %d)", \
              test, __func__, #ACTUAL, #EXPECTED, __check_vector_it);  \
        fprintf (stderr, "incorrect value\n"); \
        abort ();                              \
      } \
    } \
  pass ("%s: %s: actual: %s == expected: %s", \
        test, __func__, #ACTUAL, #EXPECTED);  \
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

#define CHECK_NO_ERRORS(CTXT) \
  do { \
    const char *err = gcc_jit_context_get_first_error (CTXT); \
    if (err) \
      fail ("%s: %s: error unexpectedly occurred: %s", test, __func__, err); \
    else \
      pass ("%s: %s: no errors occurred", test, __func__); \
  } while (0)

/* Hooks that testcases should provide.  */
extern void
create_code (gcc_jit_context *ctxt, void * user_data);

#ifndef TEST_COMPILING_TO_FILE
extern void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result);
#endif

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

#ifndef TEST_ESCHEWS_SET_OPTIONS
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
  /* Make it easier to compare error messages by disabling colorization,
     rather then have them be affected by whether stderr is going to a tty.  */
  gcc_jit_context_add_command_line_option
    (ctxt, "-fdiagnostics-color=never");
}
#endif /* #ifndef TEST_ESCHEWS_SET_OPTIONS */

/* Concatenate two strings.  The result must be released using "free".  */

char *
concat_strings (const char *prefix, const char *suffix)
{
  char *result = (char *)malloc (strlen (prefix) + strlen (suffix) + 1);
  if (!result)
    {
      fail ("malloc failure");
      return NULL;
    }
  strcpy (result, prefix);
  strcpy (result + strlen (prefix), suffix);
  result[strlen (prefix) + strlen (suffix)] = '\0';
  return result;
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
  logfile_name = concat_strings (argv0, logfile_name_suffix);
  if (!logfile_name)
    return NULL;
  logfile = fopen (logfile_name, "w");
  CHECK_NON_NULL (logfile);
  free (logfile_name);

  if (logfile)
    gcc_jit_context_set_logfile (ctxt, logfile, 0, 0);

  return logfile;
}

/* Exercise the API entrypoint:
     gcc_jit_context_dump_reproducer_to_file
   by calling it on the context, using the path expected by jit.exp.  */
static void
dump_reproducer (gcc_jit_context *ctxt, const char *argv0)
{
  char *reproducer_name;
  reproducer_name = concat_strings (argv0, ".reproducer.c");
  if (!reproducer_name)
    return;
  note ("%s: writing reproducer to %s", test, reproducer_name);
  gcc_jit_context_dump_reproducer_to_file (ctxt, reproducer_name);
  free (reproducer_name);
}

/* Run one iteration of the test.  */
static void
test_jit (const char *argv0, void *user_data)
{
  gcc_jit_context *ctxt;
  FILE *logfile;
#ifndef TEST_COMPILING_TO_FILE
  gcc_jit_result *result;
#endif

#ifdef TEST_COMPILING_TO_FILE
  unlink (OUTPUT_FILENAME);
#endif

  ctxt = gcc_jit_context_acquire ();
  if (!ctxt)
    {
      fail ("gcc_jit_context_acquire failed");
      return;
    }

  logfile = set_up_logging (ctxt, argv0);

  set_options (ctxt, argv0);

  create_code (ctxt, user_data);

  dump_reproducer (ctxt, argv0);

#ifdef TEST_COMPILING_TO_FILE
  gcc_jit_context_compile_to_file (ctxt,
				   (OUTPUT_KIND),
				   (OUTPUT_FILENAME));
  CHECK_NO_ERRORS (ctxt);
#else /* #ifdef TEST_COMPILING_TO_FILE */
  /* This actually calls into GCC and runs the build, all
     in a mutex for now.  */
  result = gcc_jit_context_compile (ctxt);

  verify_code (ctxt, result);
#endif

  gcc_jit_context_release (ctxt);

#ifndef TEST_COMPILING_TO_FILE
  /* Once we're done with the code, this unloads the built .so file: */
  gcc_jit_result_release (result);
#endif

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
