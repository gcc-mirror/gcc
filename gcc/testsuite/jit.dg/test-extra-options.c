/* Testcase for gcc_jit_context_add_command_line_option (PR jit/66628).  */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgccjit.h"

#include "harness.h"

#ifndef LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option
#error LIBGCCJIT_HAVE_gcc_jit_context_add_command_line_option was not defined
#endif

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_context_add_command_line_option (ctxt, "-ffast-math");
  gcc_jit_context_add_command_line_option (ctxt, "-fverbose-asm");

  /* Let's try to inject the equivalent of:

	double
	my_dot_product (int n, double *a, double *b)
	{
	  double result = 0.;
	  for (int i = 0; i < n; i++)
	    result += a[i] * b[i];
	  return result
	}

     and see what the optimizer can do.  */
  gcc_jit_type *val_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_DOUBLE);
  gcc_jit_type *ptr_type = gcc_jit_type_get_pointer (val_type);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_type *return_type = val_type;
  gcc_jit_param *param_n =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "n");
  gcc_jit_param *param_a =
    gcc_jit_context_new_param (ctxt, NULL, ptr_type, "a");
  gcc_jit_param *param_b =
    gcc_jit_context_new_param (ctxt, NULL, ptr_type, "b");
  gcc_jit_param *params[3] = {param_n, param_a, param_b};
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  return_type,
				  "my_dot_product",
				  3, params, 0);

  gcc_jit_block *initial = gcc_jit_function_new_block (func, "initial");
  gcc_jit_block *loop_test = gcc_jit_function_new_block (func, "loop_test");
  gcc_jit_block *loop_body = gcc_jit_function_new_block (func, "loop_body");
  gcc_jit_block *final = gcc_jit_function_new_block (func, "final");

  /* Build: "double result = 0.;" */
  gcc_jit_lvalue *result =
    gcc_jit_function_new_local (func, NULL, val_type, "result");

  gcc_jit_block_add_assignment (initial, NULL,
    result, gcc_jit_context_zero (ctxt, val_type));

  /* Build: "for (int i = 0; i < n; i++)" */
  gcc_jit_lvalue *i =
    gcc_jit_function_new_local (func, NULL, int_type, "i");
  gcc_jit_block_add_assignment (initial, NULL,
    i, gcc_jit_context_zero (ctxt, int_type));

  gcc_jit_block_end_with_jump (initial, NULL, loop_test);

  gcc_jit_block_end_with_conditional (
    loop_test, NULL,

    /* (i < n) */
    gcc_jit_context_new_comparison (
      ctxt, NULL,
      GCC_JIT_COMPARISON_LT,
      gcc_jit_lvalue_as_rvalue (i),
      gcc_jit_param_as_rvalue (param_n)),

    loop_body,
    final);

  /* Build: "result += a[i] * b[i];" */
  gcc_jit_block_add_assignment_op (
    loop_body, NULL,
    result,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      GCC_JIT_BINARY_OP_MULT,
      val_type,
      gcc_jit_lvalue_as_rvalue (
	gcc_jit_context_new_array_access (
          ctxt, NULL,
	  gcc_jit_param_as_rvalue (param_a),
	  gcc_jit_lvalue_as_rvalue (i))),
      gcc_jit_lvalue_as_rvalue (
	gcc_jit_context_new_array_access (
          ctxt, NULL,
	  gcc_jit_param_as_rvalue (param_b),
	  gcc_jit_lvalue_as_rvalue (i)))));

  /* Build: "i++" */
  gcc_jit_block_add_assignment_op (
    loop_body, NULL,
    i,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_one (ctxt, int_type));

  gcc_jit_block_end_with_jump (loop_body, NULL, loop_test);

  /* Build: "return result;" */
  gcc_jit_block_end_with_return (
    final,
    NULL,
    gcc_jit_lvalue_as_rvalue (result));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef double (*my_dot_product_fn_type) (int n, double *a, double *b);
  CHECK_NON_NULL (result);

  my_dot_product_fn_type my_dot_product =
    (my_dot_product_fn_type)gcc_jit_result_get_code (result,
						     "my_dot_product");
  CHECK_NON_NULL (my_dot_product);
  double test_array[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10.};
  double val = my_dot_product (10, test_array, test_array);
  note ("my_dot_product returned: %f", val);
  CHECK_VALUE (val, 385.0);
}
