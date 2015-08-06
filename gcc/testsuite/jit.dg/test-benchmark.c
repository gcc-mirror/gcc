/* A simple benchmark: how long does it take to use libgccjit to
   compile and run a simple function?  */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/times.h>

#include "libgccjit.h"

#define TEST_ESCHEWS_SET_OPTIONS
#define TEST_ESCHEWS_TEST_JIT
#define TEST_PROVIDES_MAIN
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /*
    Simple sum-of-squares, to test conditionals and looping

    int loop_test (int n)
    {
      int i;
      int sum = 0;
      for (i = 0; i < n ; i ++)
      {
	sum += i * i;
      }
      return sum;
   */
  gcc_jit_type *the_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *return_type = the_type;

  gcc_jit_param *n =
    gcc_jit_context_new_param (ctxt, NULL, the_type, "n");
  gcc_jit_param *params[1] = {n};
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  return_type,
				  "loop_test",
				  1, params, 0);

  /* Build locals:  */
  gcc_jit_lvalue *i =
    gcc_jit_function_new_local (func, NULL, the_type, "i");
  gcc_jit_lvalue *sum =
    gcc_jit_function_new_local (func, NULL, the_type, "sum");

  gcc_jit_block *initial =
    gcc_jit_function_new_block (func, "initial");
  gcc_jit_block *loop_cond =
    gcc_jit_function_new_block (func, "loop_cond");
  gcc_jit_block *loop_body =
    gcc_jit_function_new_block (func, "loop_body");
  gcc_jit_block *after_loop =
    gcc_jit_function_new_block (func, "after_loop");

  /* sum = 0; */
  gcc_jit_block_add_assignment (
    initial, NULL,
    sum,
    gcc_jit_context_new_rvalue_from_int (ctxt, the_type, 0));

  /* i = 0; */
  gcc_jit_block_add_assignment (
    initial, NULL,
    i,
    gcc_jit_context_new_rvalue_from_int (ctxt, the_type, 0));

  gcc_jit_block_end_with_jump (initial, NULL, loop_cond);

  /* if (i >= n) */
  gcc_jit_block_end_with_conditional (
    loop_cond, NULL,
    gcc_jit_context_new_comparison (
       ctxt, NULL,
       GCC_JIT_COMPARISON_GE,
       gcc_jit_lvalue_as_rvalue (i),
       gcc_jit_param_as_rvalue (n)),
    after_loop,
    loop_body);

  /* sum += i * i */
  gcc_jit_block_add_assignment (
    loop_body, NULL,
    sum,
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      GCC_JIT_BINARY_OP_PLUS, the_type,
      gcc_jit_lvalue_as_rvalue (sum),
      gcc_jit_context_new_binary_op (
	 ctxt, NULL,
	 GCC_JIT_BINARY_OP_MULT, the_type,
	 gcc_jit_lvalue_as_rvalue (i),
	 gcc_jit_lvalue_as_rvalue (i))));

  /* i++ */
  gcc_jit_block_add_assignment (
    loop_body, NULL,
    i,
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      GCC_JIT_BINARY_OP_PLUS, the_type,
      gcc_jit_lvalue_as_rvalue (i),
      gcc_jit_context_new_rvalue_from_int (
	ctxt,
	the_type,
	1)));

  gcc_jit_block_end_with_jump (loop_body, NULL, loop_cond);

  /* return sum */
  gcc_jit_block_end_with_return (
    after_loop,
    NULL,
    gcc_jit_lvalue_as_rvalue (sum));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*loop_test_fn_type) (int);
  if (!result)
    {
      fail ("%s: %s: !result", test, __func__);
      return;
    }
  loop_test_fn_type loop_test =
    (loop_test_fn_type)gcc_jit_result_get_code (result, "loop_test");
  if (!loop_test)
    {
      fail ("%s: %s: !loop_test", test, __func__);
      return;
    }
  int val = loop_test (100);
  if (val != 328350)
    fail ("%s: %s: val != 328350", test, __func__);
}

/* Run one iteration of the test.  */
static void
test_jit (const char *argv0, int opt_level, gcc_jit_timer *timer)
{
  gcc_jit_context *ctxt;
  gcc_jit_result *result;

  gcc_jit_timer_push (timer, "test_jit");

  ctxt = gcc_jit_context_acquire ();
  if (!ctxt)
    {
      fail ("gcc_jit_context_acquire failed");
      return;
    }

  gcc_jit_context_set_timer (ctxt, timer);

  /* Set up options.  */
  gcc_jit_context_set_str_option (
    ctxt,
    GCC_JIT_STR_OPTION_PROGNAME,
    argv0);

  /* Set up options for benchmarking.  */
  gcc_jit_context_set_int_option (
    ctxt,
    GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL,
    opt_level);
  /* Generating debuginfo takes time; turn it off.  */
  gcc_jit_context_set_bool_option (
    ctxt,
    GCC_JIT_BOOL_OPTION_DEBUGINFO,
    0);
  /* This option is extremely slow; turn it off.  */
  gcc_jit_context_set_bool_option (
    ctxt,
    GCC_JIT_BOOL_OPTION_SELFCHECK_GC,
    0);

  /* Turn this on to get detailed timings.  */
  if (0)
    gcc_jit_context_set_bool_option (
      ctxt,
      GCC_JIT_BOOL_OPTION_DUMP_SUMMARY,
      1);

  gcc_jit_timer_push (timer, "create_code");
  create_code (ctxt, NULL);
  gcc_jit_timer_pop (timer, "create_code");

  gcc_jit_timer_push (timer, "compile");
  result = gcc_jit_context_compile (ctxt);
  gcc_jit_timer_pop (timer, "compile");

  gcc_jit_timer_push (timer, "verify_code");
  verify_code (ctxt, result);
  gcc_jit_timer_pop (timer, "verify_code");

  gcc_jit_context_release (ctxt);
  gcc_jit_result_release (result);

  gcc_jit_timer_pop (timer, "test_jit");
}

/* Taken from timevar.c.  */
static double ticks_to_msec;
#define TICKS_PER_SECOND sysconf (_SC_CLK_TCK) /* POSIX 1003.1-1996 */
#define TICKS_TO_MSEC (1 / (double)TICKS_PER_SECOND)
static double get_wallclock_time (void)
{
  struct tms tms;
  return times (&tms) * ticks_to_msec;
}

/* Time 100 iterations, at each optimization level
   (for 400 iterations in all).  */

int
main (int argc, char **argv)
{
  int opt_level;
  int num_iterations = 100;
  double elapsed_time[4];

  ticks_to_msec = TICKS_TO_MSEC;

  for (opt_level = 0; opt_level < 4; opt_level++)
    {
      int i;
      double start_time, end_time;
      start_time = get_wallclock_time ();
      gcc_jit_timer *timer = gcc_jit_timer_new ();
      for (i = 1; i <= num_iterations; i++)
	{
	  snprintf (test, sizeof (test),
		    "%s iteration %d of %d",
		    extract_progname (argv[0]),
		    i, num_iterations);
	  test_jit (argv[0], opt_level, timer);
	}
      end_time = get_wallclock_time ();
      elapsed_time[opt_level] = end_time - start_time;
      gcc_jit_timer_print (timer, stderr);
      gcc_jit_timer_release (timer);
      pass ("%s: survived %i iterations at optlevel %i",
	    argv[0], num_iterations, opt_level);
      note (("%s: %i iterations at optlevel %i"
	     " took a total of %.3fs (%.3fs per iteration)"),
	    argv[0], num_iterations, opt_level,
	    elapsed_time[opt_level],
	    elapsed_time[opt_level] / num_iterations);
    }

  totals ();

  /* Print a summary.  */
  printf ("%s: %i iterations: time taken (lower is better)\n",
	  argv[0], num_iterations);
  for (opt_level = 0; opt_level < 4; opt_level++)
    printf ("optlevel %i: %.3fs (%.3fs per iteration)\n",
	    opt_level,
	    elapsed_time[opt_level],
	    elapsed_time[opt_level] / num_iterations);

  return 0;
}
