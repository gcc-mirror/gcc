/* Usage example for libgccjit.so
   Copyright (C) 2014-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include <libgccjit.h>

#include <stdlib.h>
#include <stdio.h>

void
create_code (gcc_jit_context *ctxt)
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

  gcc_jit_block *b_initial =
    gcc_jit_function_new_block (func, "initial");
  gcc_jit_block *b_loop_cond =
    gcc_jit_function_new_block (func, "loop_cond");
  gcc_jit_block *b_loop_body =
    gcc_jit_function_new_block (func, "loop_body");
  gcc_jit_block *b_after_loop =
    gcc_jit_function_new_block (func, "after_loop");

  /* sum = 0; */
  gcc_jit_block_add_assignment (
    b_initial, NULL,
    sum,
    gcc_jit_context_zero (ctxt, the_type));

  /* i = 0; */
  gcc_jit_block_add_assignment (
    b_initial, NULL,
    i,
    gcc_jit_context_zero (ctxt, the_type));

  gcc_jit_block_end_with_jump (b_initial, NULL, b_loop_cond);

  /* if (i >= n) */
  gcc_jit_block_end_with_conditional (
    b_loop_cond, NULL,
    gcc_jit_context_new_comparison (
       ctxt, NULL,
       GCC_JIT_COMPARISON_GE,
       gcc_jit_lvalue_as_rvalue (i),
       gcc_jit_param_as_rvalue (n)),
    b_after_loop,
    b_loop_body);

  /* sum += i * i */
  gcc_jit_block_add_assignment_op (
    b_loop_body, NULL,
    sum,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      GCC_JIT_BINARY_OP_MULT, the_type,
      gcc_jit_lvalue_as_rvalue (i),
      gcc_jit_lvalue_as_rvalue (i)));

  /* i++ */
  gcc_jit_block_add_assignment_op (
    b_loop_body, NULL,
    i,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_one (ctxt, the_type));

  gcc_jit_block_end_with_jump (b_loop_body, NULL, b_loop_cond);

  /* return sum */
  gcc_jit_block_end_with_return (
    b_after_loop,
    NULL,
    gcc_jit_lvalue_as_rvalue (sum));
}

int
main (int argc, char **argv)
{
  gcc_jit_context *ctxt = NULL;
  gcc_jit_result *result = NULL;

  /* Get a "context" object for working with the library.  */
  ctxt = gcc_jit_context_acquire ();
  if (!ctxt)
    {
      fprintf (stderr, "NULL ctxt");
      goto error;
    }

  /* Set some options on the context.
     Let's see the code being generated, in assembler form.  */
  gcc_jit_context_set_bool_option (
    ctxt,
    GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE,
    0);

  /* Populate the context.  */
  create_code (ctxt);

  /* Compile the code.  */
  result = gcc_jit_context_compile (ctxt);
  if (!result)
    {
      fprintf (stderr, "NULL result");
      goto error;
    }

  /* Extract the generated code from "result".  */
  typedef int (*loop_test_fn_type) (int);
  loop_test_fn_type loop_test =
    (loop_test_fn_type)gcc_jit_result_get_code (result, "loop_test");
  if (!loop_test)
    {
      fprintf (stderr, "NULL loop_test");
      goto error;
    }

  /* Run the generated code.  */
  int val = loop_test (10);
  printf("loop_test returned: %d\n", val);

 error:
  gcc_jit_context_release (ctxt);
  gcc_jit_result_release (result);
  return 0;
}
