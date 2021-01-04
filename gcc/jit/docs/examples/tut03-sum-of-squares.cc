/* Usage example for libgccjit.so's C++ API
   Copyright (C) 2014-2021 Free Software Foundation, Inc.

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

#include <libgccjit++.h>

#include <stdlib.h>
#include <stdio.h>

void
create_code (gccjit::context ctxt)
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
  gccjit::type the_type = ctxt.get_int_type <int> ();
  gccjit::type return_type = the_type;

  gccjit::param n = ctxt.new_param (the_type, "n");
  std::vector<gccjit::param> params;
  params.push_back (n);
  gccjit::function func =
    ctxt.new_function (GCC_JIT_FUNCTION_EXPORTED,
                       return_type,
                       "loop_test",
                       params, 0);

  /* Build locals:  */
  gccjit::lvalue i = func.new_local (the_type, "i");
  gccjit::lvalue sum = func.new_local (the_type, "sum");

  gccjit::block b_initial = func.new_block ("initial");
  gccjit::block b_loop_cond = func.new_block ("loop_cond");
  gccjit::block b_loop_body = func.new_block ("loop_body");
  gccjit::block b_after_loop = func.new_block ("after_loop");

  /* sum = 0; */
  b_initial.add_assignment (sum, ctxt.zero (the_type));

  /* i = 0; */
  b_initial.add_assignment (i, ctxt.zero (the_type));

  b_initial.end_with_jump (b_loop_cond);

  /* if (i >= n) */
  b_loop_cond.end_with_conditional (
    i >= n,
    b_after_loop,
    b_loop_body);

  /* sum += i * i */
  b_loop_body.add_assignment_op (sum,
                                 GCC_JIT_BINARY_OP_PLUS,
                                 i * i);

  /* i++ */
  b_loop_body.add_assignment_op (i,
                                GCC_JIT_BINARY_OP_PLUS,
                                ctxt.one (the_type));

  b_loop_body.end_with_jump (b_loop_cond);

  /* return sum */
  b_after_loop.end_with_return (sum);
}

int
main (int argc, char **argv)
{
  gccjit::context ctxt;
  gcc_jit_result *result = NULL;

  /* Get a "context" object for working with the library.  */
  ctxt = gccjit::context::acquire ();

  /* Set some options on the context.
     Turn this on to see the code being generated, in assembler form.  */
  ctxt.set_bool_option (GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE,
                        0);

  /* Populate the context.  */
  create_code (ctxt);

  /* Compile the code.  */
  result = ctxt.compile ();

  ctxt.release ();

  if (!result)
    {
      fprintf (stderr, "NULL result");
      return 1;
    }

  /* Extract the generated code from "result".  */
  typedef int (*loop_test_fn_type) (int);
  loop_test_fn_type loop_test =
    (loop_test_fn_type)gcc_jit_result_get_code (result, "loop_test");
  if (!loop_test)
    {
      fprintf (stderr, "NULL loop_test");
      gcc_jit_result_release (result);
      return 1;
    }

  /* Run the generated code.  */
  int val = loop_test (10);
  printf("loop_test returned: %d\n", val);

  gcc_jit_result_release (result);
  return 0;
}
