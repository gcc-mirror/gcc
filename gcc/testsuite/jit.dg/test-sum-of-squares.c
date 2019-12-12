#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgccjit.h"

#include "harness.h"

static char *dump_vrp1;

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
  gcc_jit_context_enable_dump (ctxt, "tree-vrp1", &dump_vrp1);

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
  CHECK_NON_NULL (result);
  loop_test_fn_type loop_test =
    (loop_test_fn_type)gcc_jit_result_get_code (result, "loop_test");
  CHECK_NON_NULL (loop_test);
  int val = loop_test (10);
  note ("loop_test returned: %d", val);
  CHECK_VALUE (val, 285);

  CHECK_NON_NULL (dump_vrp1);
  /* PR jit/64166
     An example of using gcc_jit_context_enable_dump to verify a property
     of the compile.

     In this case, verify that vrp is able to deduce the
     bounds of the iteration variable. Specifically, verify that some
     variable is known to be in the range negative infinity to some
     expression based on param "n" (actually n-1).  */
  CHECK_STRING_CONTAINS (dump_vrp1, "[-INF, n_");
  free (dump_vrp1);
}
