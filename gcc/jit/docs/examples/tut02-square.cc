/* Usage example for libgccjit.so's C++ API
   Copyright (C) 2014-2016 Free Software Foundation, Inc.

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
  /* Let's try to inject the equivalent of this C code:

      int square (int i)
      {
        return i * i;
      }
  */
  gccjit::type int_type = ctxt.get_type (GCC_JIT_TYPE_INT);
  gccjit::param param_i = ctxt.new_param (int_type, "i");
  std::vector<gccjit::param> params;
  params.push_back (param_i);
  gccjit::function func = ctxt.new_function (GCC_JIT_FUNCTION_EXPORTED,
                                             int_type,
                                             "square",
                                             params, 0);

  gccjit::block block = func.new_block ();

  gccjit::rvalue expr =
    ctxt.new_binary_op (GCC_JIT_BINARY_OP_MULT, int_type,
                        param_i, param_i);

  block.end_with_return (expr);
}

int
main (int argc, char **argv)
{
  /* Get a "context" object for working with the library.  */
  gccjit::context ctxt = gccjit::context::acquire ();

  /* Set some options on the context.
     Turn this on to see the code being generated, in assembler form.  */
  ctxt.set_bool_option (
    GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE,
    0);

  /* Populate the context.  */
  create_code (ctxt);

  /* Compile the code.  */
  gcc_jit_result *result = ctxt.compile ();

  /* We're done with the context; we can release it: */
  ctxt.release ();

  if (!result)
    {
      fprintf (stderr, "NULL result");
      return 1;
    }

  /* Extract the generated code from "result".  */
  void *fn_ptr = gcc_jit_result_get_code (result, "square");
  if (!fn_ptr)
     {
       fprintf (stderr, "NULL fn_ptr");
       gcc_jit_result_release (result);
       return 1;
     }

  typedef int (*fn_type) (int);
  fn_type square = (fn_type)fn_ptr;
  printf ("result: %d\n", square (5));

  gcc_jit_result_release (result);
  return 0;
}
