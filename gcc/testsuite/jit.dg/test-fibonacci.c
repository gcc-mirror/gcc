#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  const int FIRST_LINE = __LINE__ + 4;
  /* Let's try to inject the equivalent of:
0000000001111111111222222222233333333334444444444555555555566666666667
1234567890123456789012345678901234567890123456789012345678901234567890
FIRST_LINE + 0: int
FIRST_LINE + 1: my_fibonacci (int x)
FIRST_LINE + 2: {
FIRST_LINE + 3:   if (x < 2)
FIRST_LINE + 4:     return x;
FIRST_LINE + 5:   else
FIRST_LINE + 6:     return my_fibonacci (x - 1) + my_fibonacci (x - 2);
FIRST_LINE + 7: }
0000000001111111111222222222233333333334444444444555555555566666666667
1234567890123456789012345678901234567890123456789012345678901234567890

     where the source locations are set up to point to the commented-out
     code above.
     It should therefore be possible to step through the generated code
     in the debugger, stepping through the above commented-out code
     fragement.
   */
  gcc_jit_type *the_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *return_type = the_type;

  gcc_jit_param *x =
    gcc_jit_context_new_param (
      ctxt,
      gcc_jit_context_new_location (
	ctxt, __FILE__, FIRST_LINE + 1, 35),
      the_type, "x");
  gcc_jit_param *params[1] = {x};
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt,
				  gcc_jit_context_new_location (
				    ctxt, __FILE__, FIRST_LINE, 17),
				  GCC_JIT_FUNCTION_EXPORTED,
				  return_type,
				  "my_fibonacci",
				  1, params, 0);

  gcc_jit_block *initial =
    gcc_jit_function_new_block (func, "initial");
  gcc_jit_block *on_true =
    gcc_jit_function_new_block (func, "on_true");
  gcc_jit_block *on_false =
    gcc_jit_function_new_block (func, "on_false");

  /* if (x < 2) */
  gcc_jit_block_end_with_conditional (
    initial,
    gcc_jit_context_new_location (ctxt, __FILE__, FIRST_LINE + 3, 19),
    gcc_jit_context_new_comparison (
      ctxt,
      gcc_jit_context_new_location (ctxt, __FILE__, FIRST_LINE + 3, 25),
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
    gcc_jit_context_new_location (ctxt, __FILE__, FIRST_LINE + 4, 21),
    gcc_jit_param_as_rvalue (x));

  /* false branch: */
  gcc_jit_rvalue *x_minus_1 =
    gcc_jit_context_new_binary_op (
      ctxt,
      gcc_jit_context_new_location (ctxt, __FILE__, FIRST_LINE + 6, 44),
      GCC_JIT_BINARY_OP_MINUS, the_type,
      gcc_jit_param_as_rvalue (x),
      gcc_jit_context_new_rvalue_from_int (
	ctxt,
	the_type,
	1));
  gcc_jit_rvalue *x_minus_2 =
    gcc_jit_context_new_binary_op (
      ctxt,
      gcc_jit_context_new_location (ctxt, __FILE__, FIRST_LINE + 6, 67),
      GCC_JIT_BINARY_OP_MINUS, the_type,
      gcc_jit_param_as_rvalue (x),
      gcc_jit_context_new_rvalue_from_int (
	ctxt,
	the_type,
	2));
  gcc_jit_block_end_with_return (
    on_false,
    gcc_jit_context_new_location (ctxt, __FILE__, FIRST_LINE + 6, 21),
    gcc_jit_context_new_binary_op (
      ctxt,
      gcc_jit_context_new_location (ctxt, __FILE__, FIRST_LINE + 6, 49),
      GCC_JIT_BINARY_OP_PLUS, the_type,
      /* my_fibonacci (x - 1) */
      gcc_jit_context_new_call (
	ctxt,
	gcc_jit_context_new_location (ctxt, __FILE__, FIRST_LINE + 6, 28),
	func,
	1, &x_minus_1),
      /* my_fibonacci (x - 2) */
      gcc_jit_context_new_call (
	ctxt,
	gcc_jit_context_new_location (ctxt, __FILE__, FIRST_LINE + 6, 51),
	func,
	1, &x_minus_2)));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*my_fibonacci_fn_type) (int);
  CHECK_NON_NULL (result);
  my_fibonacci_fn_type my_fibonacci =
    (my_fibonacci_fn_type)gcc_jit_result_get_code (result, "my_fibonacci");
  CHECK_NON_NULL (my_fibonacci);
  int val = my_fibonacci (10);
  note ("my_fibonacci returned: %d", val);
  CHECK_VALUE (val, 55);
}
