#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgccjit.h"

#include "harness.h"

/* Quote from here in docs/topics/functions.rst.  */

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
      int
      test_switch (int x)
      {
	switch (x)
	  {
	  case 0 ... 5:
	     return 3;

	  case 25 ... 27:
	     return 4;

	  case -42 ... -17:
	     return 83;

	  case 40:
	     return 8;

	  default:
	     return 10;
	  }
      }
   */
  gcc_jit_type *t_int =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *return_type = t_int;
  gcc_jit_param *x =
    gcc_jit_context_new_param (ctxt, NULL, t_int, "x");
  gcc_jit_param *params[1] = {x};
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  return_type,
				  "test_switch",
				  1, params, 0);

  gcc_jit_block *b_initial =
    gcc_jit_function_new_block (func, "initial");

  gcc_jit_block *b_default =
    gcc_jit_function_new_block (func, "default");
  gcc_jit_block *b_case_0_5 =
    gcc_jit_function_new_block (func, "case_0_5");
  gcc_jit_block *b_case_25_27 =
    gcc_jit_function_new_block (func, "case_25_27");
  gcc_jit_block *b_case_m42_m17 =
    gcc_jit_function_new_block (func, "case_m42_m17");
  gcc_jit_block *b_case_40 =
    gcc_jit_function_new_block (func, "case_40");

  gcc_jit_case *cases[4] = {
    gcc_jit_context_new_case (
      ctxt,
      gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 0),
      gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 5),
      b_case_0_5),
    gcc_jit_context_new_case (
      ctxt,
      gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 25),
      gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 27),
      b_case_25_27),
    gcc_jit_context_new_case (
      ctxt,
      gcc_jit_context_new_rvalue_from_int (ctxt, t_int, -42),
      gcc_jit_context_new_rvalue_from_int (ctxt, t_int, -17),
      b_case_m42_m17),
    gcc_jit_context_new_case (
      ctxt,
      gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 40),
      gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 40),
      b_case_40)
  };
  gcc_jit_block_end_with_switch (
    b_initial, NULL,
    gcc_jit_param_as_rvalue (x),
    b_default,
    4, cases);

  gcc_jit_block_end_with_return (
    b_case_0_5, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 3));
  gcc_jit_block_end_with_return (
    b_case_25_27, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 4));
  gcc_jit_block_end_with_return (
    b_case_m42_m17, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 83));
  gcc_jit_block_end_with_return (
    b_case_40, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 8));
  gcc_jit_block_end_with_return (
    b_default, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 10));
}

/* Quote up to here in docs/topics/functions.rst.  */

static int
c_test_switch (int x)
{
  switch (x)
    {
    case 0 ... 5:
      return 3;
    case 25 ... 27:
      return 4;
    case -42 ... -17:
      return 83;
    case 40:
      return 8;
    default:
      return 10;
    }
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*test_switch_type) (int);
  CHECK_NON_NULL (result);
  test_switch_type test_switch =
    (test_switch_type)gcc_jit_result_get_code (result, "test_switch");
  CHECK_NON_NULL (test_switch);

  int i;

  for (i = -255; i < 255; i++)
    {
      int val = test_switch (i);
      int exp = c_test_switch (i);
      if (val != exp)
	fail ("test_switch (%i) returned: %i; expected; %i", i, val, exp);
    }
}
