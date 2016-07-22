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
      my_factorial_must_tail_call (int x)
      {
        if (x < 2)
          return x;
        else
          return x * my_factorial_must_tail_call (x - 1);
      }

     and mark the call as requiring tail-call-optimization.
   */
  gcc_jit_type *the_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *return_type = the_type;

  gcc_jit_param *x =
    gcc_jit_context_new_param (ctxt, NULL, the_type, "x");
  gcc_jit_param *params[1] = {x};
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  return_type,
				  "my_factorial_must_tail_call",
				  1, params, 0);

  gcc_jit_block *initial =
    gcc_jit_function_new_block (func, "initial");
  gcc_jit_block *on_true =
    gcc_jit_function_new_block (func, "on_true");
  gcc_jit_block *on_false =
    gcc_jit_function_new_block (func, "on_false");

 /* if (x < 2) */
  gcc_jit_block_end_with_conditional (
    initial, NULL,
    gcc_jit_context_new_comparison (
      ctxt, NULL,
      GCC_JIT_COMPARISON_LT,
      gcc_jit_param_as_rvalue (x),
      gcc_jit_context_new_rvalue_from_int (
	ctxt,
	the_type,
	2)),
    on_true,
    on_false);

  /* true branch: */
  /* return x */
  gcc_jit_block_end_with_return (
    on_true,
    NULL,
    gcc_jit_param_as_rvalue (x));

  /* false branch: */
  gcc_jit_rvalue *x_minus_1 =
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      GCC_JIT_BINARY_OP_MINUS, the_type,
      gcc_jit_param_as_rvalue (x),
      gcc_jit_context_new_rvalue_from_int (
	ctxt,
	the_type,
	1));
  /* my_factorial_must_tail_call (x - 1) */
  gcc_jit_rvalue *call =
      gcc_jit_context_new_call (
        ctxt, NULL,
        func,
        1, &x_minus_1);

  /* Mark the call as requiring tail-call optimization.  */
  gcc_jit_rvalue_set_bool_require_tail_call (call, 1);

  gcc_jit_block_end_with_return (
    on_false,
    NULL,
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      GCC_JIT_BINARY_OP_MULT, the_type,
      gcc_jit_param_as_rvalue (x),
      call));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*my_factorial_fn_type) (int);
  CHECK_NON_NULL (result);
  my_factorial_fn_type my_factorial_must_tail_call =
    (my_factorial_fn_type)gcc_jit_result_get_code (result, "my_factorial_must_tail_call");
  CHECK_NON_NULL (my_factorial_must_tail_call);
  int val = my_factorial_must_tail_call (10);
  note ("my_factorial_must_tail_call returned: %d", val);
  CHECK_VALUE (val, 3628800);
}

