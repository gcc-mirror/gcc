#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

	double
	test_nested_loops (int n, double *a, double *b)
	{
	  double result = 0.;
	  for (int i = 0; i < n; i++)
	    for (int j = 0; j < n; j++)
	      result += a[i] * b[j];
	  return result
	}
  */
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
				  "test_nested_loops",
				  3, params, 0);

  /* Create locals. */
  gcc_jit_lvalue *result =
    gcc_jit_function_new_local (func, NULL, val_type, "result");
  gcc_jit_lvalue *i =
    gcc_jit_function_new_local (func, NULL, int_type, "i");
  gcc_jit_lvalue *j =
    gcc_jit_function_new_local (func, NULL, int_type, "j");

  /* Create basic blocks. */
  gcc_jit_block *b_entry =
    gcc_jit_function_new_block (func, "b_entry");
  gcc_jit_block *b_outer_loop_cond =
    gcc_jit_function_new_block (func, "b_outer_loop_cond");
  gcc_jit_block *b_outer_loop_head =
    gcc_jit_function_new_block (func, "b_outer_loop_head");
  gcc_jit_block *b_outer_loop_tail =
    gcc_jit_function_new_block (func, "b_outer_loop_tail");
  gcc_jit_block *b_inner_loop_cond =
    gcc_jit_function_new_block (func, "b_inner_loop_cond");
  gcc_jit_block *b_inner_loop_body =
    gcc_jit_function_new_block (func, "b_inner_loop_body");
  gcc_jit_block *b_exit =
    gcc_jit_function_new_block (func, "b_exit");


  /* Populate b_entry. */

  /* "result = 0.;" */
  gcc_jit_block_add_assignment (
    b_entry, NULL,
    result,
    gcc_jit_context_zero (ctxt, val_type));
  /* "i = 0;" */
  gcc_jit_block_add_assignment (
    b_entry, NULL,
    i,
    gcc_jit_context_zero (ctxt, int_type));
  gcc_jit_block_end_with_jump (b_entry, NULL, b_outer_loop_cond);

  /* Populate b_outer_loop_cond. */
  gcc_jit_block_end_with_conditional (
    b_outer_loop_cond,
    NULL,
    /* (i < n) */
    gcc_jit_context_new_comparison (
      ctxt, NULL,
      GCC_JIT_COMPARISON_LT,
      gcc_jit_lvalue_as_rvalue (i),
      gcc_jit_param_as_rvalue (param_n)),
    b_outer_loop_head,
    b_exit);

  /* Populate b_outer_loop_head. */
  /* j = 0; */
  gcc_jit_block_add_assignment (
    b_outer_loop_head, NULL,
    j,
    gcc_jit_context_zero (ctxt, int_type));
  gcc_jit_block_end_with_jump (b_outer_loop_head, NULL, b_inner_loop_cond);

  /* Populate b_inner_loop_cond. */
  gcc_jit_block_end_with_conditional (
    b_inner_loop_cond,
    NULL,
    /* (j < n) */
    gcc_jit_context_new_comparison (
      ctxt, NULL,
      GCC_JIT_COMPARISON_LT,
      gcc_jit_lvalue_as_rvalue (j),
      gcc_jit_param_as_rvalue (param_n)),
    b_inner_loop_body,
    b_outer_loop_tail);

  /* Populate b_inner_loop_body. */
  /* "result += a[i] * b[j];" */
  gcc_jit_block_add_assignment_op (
    b_inner_loop_body, NULL,
    result,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      GCC_JIT_BINARY_OP_MULT,
      val_type,
      gcc_jit_lvalue_as_rvalue (
        gcc_jit_context_new_array_access(
          ctxt, NULL,
          gcc_jit_param_as_rvalue (param_a),
          gcc_jit_lvalue_as_rvalue (i))),
      gcc_jit_lvalue_as_rvalue (
        gcc_jit_context_new_array_access(
          ctxt, NULL,
          gcc_jit_param_as_rvalue (param_b),
          gcc_jit_lvalue_as_rvalue (j)))));
  /* "j++" */
  gcc_jit_block_add_assignment_op (
    b_inner_loop_body, NULL,
    j,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_one (ctxt, int_type));

  gcc_jit_block_end_with_jump (b_inner_loop_body, NULL, b_inner_loop_cond);

  /* Populate b_outer_loop_tail. */
  /* "i++" */
  gcc_jit_block_add_assignment_op (
    b_outer_loop_tail, NULL,
    i,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_one (ctxt, int_type));
  gcc_jit_block_end_with_jump (b_outer_loop_tail, NULL, b_outer_loop_cond);

  /* Populate b_exit. */
  /* "return result;" */
  gcc_jit_block_end_with_return (
    b_exit,
    NULL,
    gcc_jit_lvalue_as_rvalue (result));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef double (*test_nested_loops_fn_type) (int n, double *a, double *b);
  CHECK_NON_NULL (result);

  test_nested_loops_fn_type test_nested_loops =
    (test_nested_loops_fn_type)gcc_jit_result_get_code (result,
						     "test_nested_loops");
  CHECK_NON_NULL (test_nested_loops);
  double test_a[] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10.};
  double test_b[] = {5., 6., 7., 8., 9., 10., 1., 2., 3., 4.};
  double val = test_nested_loops (10, test_a, test_b);
  note ("test_nested_loops returned: %f", val);
  CHECK_VALUE (val, 3025.0);
}
