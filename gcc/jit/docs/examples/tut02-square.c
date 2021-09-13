/* Usage example for libgccjit.so
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

#include <libgccjit.h>

#include <stdlib.h>
#include <stdio.h>

void
create_code (gcc_jit_context *ctxt)
{
  /* Let's try to inject the equivalent of:

      int square (int i)
      {
        return i * i;
      }
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_param *param_i =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "i");
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  int_type,
                                  "square",
                                  1, &param_i,
                                  0);

  gcc_jit_block *block = gcc_jit_function_new_block (func, NULL);

  gcc_jit_rvalue *expr =
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      GCC_JIT_BINARY_OP_MULT, int_type,
      gcc_jit_param_as_rvalue (param_i),
      gcc_jit_param_as_rvalue (param_i));

   gcc_jit_block_end_with_return (block, NULL, expr);
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

  /* We're done with the context; we can release it: */
  gcc_jit_context_release (ctxt);
  ctxt = NULL;

  /* Extract the generated code from "result".  */
  void *fn_ptr = gcc_jit_result_get_code (result, "square");
  if (!fn_ptr)
     {
       fprintf (stderr, "NULL fn_ptr");
       goto error;
     }

  typedef int (*fn_type) (int);
  fn_type square = (fn_type)fn_ptr;
  printf ("result: %d\n", square (5));

 error:
  if (ctxt)
    gcc_jit_context_release (ctxt);
  if (result)
    gcc_jit_result_release (result);
  return 0;
}
