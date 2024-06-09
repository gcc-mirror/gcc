#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
int
popcount (unsigned int x)
{
  int i = 0;
  while (x)
    {
      x &= x - 1;
      ++i;
    }
  return i;
}
   */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *uint_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UNSIGNED_INT);

  gcc_jit_param *param_x =
    gcc_jit_context_new_param (
      ctxt,
      NULL,
      uint_type, "x");
  gcc_jit_param *params[1] = {param_x};
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt,
				  NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "popcount",
				  1, params, 0);

  gcc_jit_lvalue *x = gcc_jit_param_as_lvalue (param_x);
  gcc_jit_rvalue *x_rvalue = gcc_jit_lvalue_as_rvalue (x);
  gcc_jit_lvalue *i =
    gcc_jit_function_new_local (func, NULL, int_type, "i");
  gcc_jit_rvalue *zero = gcc_jit_context_zero (ctxt, int_type);

  gcc_jit_block *initial =
    gcc_jit_function_new_block (func, "initial");
  gcc_jit_block *while_block =
    gcc_jit_function_new_block (func, "while");

  gcc_jit_block_add_assignment (initial, NULL, i, zero);
  gcc_jit_block_end_with_jump (initial, NULL, while_block);

  gcc_jit_block *after =
    gcc_jit_function_new_block (func, "after");

  gcc_jit_block *while_body =
    gcc_jit_function_new_block (func, "while_body");
  gcc_jit_rvalue *uzero = gcc_jit_context_zero (ctxt, uint_type);
  gcc_jit_rvalue *cmp =
    gcc_jit_context_new_comparison (ctxt, NULL, GCC_JIT_COMPARISON_NE, x_rvalue, uzero);
  gcc_jit_block_end_with_conditional (while_block, NULL, cmp, while_body, after);

  gcc_jit_rvalue *uone = gcc_jit_context_one (ctxt, uint_type);
  gcc_jit_rvalue *sub = gcc_jit_context_new_binary_op (ctxt, NULL, GCC_JIT_BINARY_OP_MINUS, uint_type, x_rvalue, uone);
  gcc_jit_block_add_assignment_op (while_body, NULL, x, GCC_JIT_BINARY_OP_BITWISE_AND, sub);

  gcc_jit_rvalue *one = gcc_jit_context_one (ctxt, int_type);
  gcc_jit_block_add_assignment_op (while_body, NULL, i, GCC_JIT_BINARY_OP_PLUS, one);
  gcc_jit_block_end_with_jump (while_body, NULL, while_block);

  gcc_jit_block_end_with_return(after, NULL, gcc_jit_lvalue_as_rvalue (i));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);
}
