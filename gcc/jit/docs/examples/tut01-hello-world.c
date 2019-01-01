/* Smoketest example for libgccjit.so
   Copyright (C) 2014-2019 Free Software Foundation, Inc.

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

static void
create_code (gcc_jit_context *ctxt)
{
  /* Let's try to inject the equivalent of:
     void
     greet (const char *name)
     {
        printf ("hello %s\n", name);
     }
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *const_char_ptr_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CONST_CHAR_PTR);
  gcc_jit_param *param_name =
    gcc_jit_context_new_param (ctxt, NULL, const_char_ptr_type, "name");
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type,
                                  "greet",
                                  1, &param_name,
                                  0);

  gcc_jit_param *param_format =
    gcc_jit_context_new_param (ctxt, NULL, const_char_ptr_type, "format");
  gcc_jit_function *printf_func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_IMPORTED,
				  gcc_jit_context_get_type (
				     ctxt, GCC_JIT_TYPE_INT),
				  "printf",
				  1, &param_format,
				  1);
  gcc_jit_rvalue *args[2];
  args[0] = gcc_jit_context_new_string_literal (ctxt, "hello %s\n");
  args[1] = gcc_jit_param_as_rvalue (param_name);

  gcc_jit_block *block = gcc_jit_function_new_block (func, NULL);

  gcc_jit_block_add_eval (
    block, NULL,
    gcc_jit_context_new_call (ctxt,
                              NULL,
                              printf_func,
                              2, args));
  gcc_jit_block_end_with_void_return (block, NULL);
}

int
main (int argc, char **argv)
{
  gcc_jit_context *ctxt;
  gcc_jit_result *result;

  /* Get a "context" object for working with the library.  */
  ctxt = gcc_jit_context_acquire ();
  if (!ctxt)
    {
      fprintf (stderr, "NULL ctxt");
      exit (1);
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
      exit (1);
    }

  /* Extract the generated code from "result".  */
  typedef void (*fn_type) (const char *);
  fn_type greet =
    (fn_type)gcc_jit_result_get_code (result, "greet");
  if (!greet)
    {
      fprintf (stderr, "NULL greet");
      exit (1);
    }

  /* Now call the generated function: */
  greet ("world");
  fflush (stdout);

  gcc_jit_context_release (ctxt);
  gcc_jit_result_release (result);
  return 0;
}
