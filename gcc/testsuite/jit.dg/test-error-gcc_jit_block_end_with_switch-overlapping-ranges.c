#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgccjit.h"

#include "harness.h"

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

	  case 5 ... 10:
	     return 4;

	  default:
	     return 10;
	  }
      }
     and verify that we get an error about the overlapping
     ranges.
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
  gcc_jit_block *b_case_5_10 =
    gcc_jit_function_new_block (func, "case_5_10");

  gcc_jit_case *cases[2] = {
    gcc_jit_context_new_case (
      ctxt,
      gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 0),
      gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 5),
      b_case_0_5),
    gcc_jit_context_new_case (
      ctxt,
      gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 5),
      gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 10),
      b_case_5_10)
  };

  gcc_jit_block_end_with_switch (
    b_initial, NULL,
    gcc_jit_param_as_rvalue (x),
    b_default,
    2,
    cases);

  gcc_jit_block_end_with_return (
    b_case_0_5, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 3));
  gcc_jit_block_end_with_return (
    b_case_5_10, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 4));
  gcc_jit_block_end_with_return (
    b_default, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, t_int, 10));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_block_end_with_switch:"
		      " duplicate (or overlapping) cases values:"
		      " case 1: case (int)5 ... (int)10: goto case_5_10;"
		      " overlaps case (int)0 ... (int)5: goto case_0_5;");
}
