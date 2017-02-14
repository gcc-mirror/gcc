/* Smoketest example for libgccjit.so C++ API
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

#include <libgccjit++.h>

#include <stdlib.h>
#include <stdio.h>

static void
create_code (gccjit::context ctxt)
{
  /* Let's try to inject the equivalent of this C code:
     void
     greet (const char *name)
     {
        printf ("hello %s\n", name);
     }
  */
  gccjit::type void_type = ctxt.get_type (GCC_JIT_TYPE_VOID);
  gccjit::type const_char_ptr_type =
    ctxt.get_type (GCC_JIT_TYPE_CONST_CHAR_PTR);
  gccjit::param param_name =
    ctxt.new_param (const_char_ptr_type, "name");
  std::vector<gccjit::param> func_params;
  func_params.push_back (param_name);
  gccjit::function func =
    ctxt.new_function (GCC_JIT_FUNCTION_EXPORTED,
                       void_type,
                       "greet",
                       func_params, 0);

  gccjit::param param_format =
    ctxt.new_param (const_char_ptr_type, "format");
  std::vector<gccjit::param> printf_params;
  printf_params.push_back (param_format);
  gccjit::function printf_func =
    ctxt.new_function (GCC_JIT_FUNCTION_IMPORTED,
                       ctxt.get_type (GCC_JIT_TYPE_INT),
                       "printf",
                       printf_params, 1);

  gccjit::block block = func.new_block ();
  block.add_eval (ctxt.new_call (printf_func,
                                 ctxt.new_rvalue ("hello %s\n"),
                                 param_name));
  block.end_with_return ();
}

int
main (int argc, char **argv)
{
  gccjit::context ctxt;
  gcc_jit_result *result;

  /* Get a "context" object for working with the library.  */
  ctxt = gccjit::context::acquire ();

  /* Set some options on the context.
     Turn this on to see the code being generated, in assembler form.  */
  ctxt.set_bool_option (GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE, 0);

  /* Populate the context.  */
  create_code (ctxt);

  /* Compile the code.  */
  result = ctxt.compile ();
  if (!result)
    {
      fprintf (stderr, "NULL result");
      exit (1);
    }

  ctxt.release ();

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

  gcc_jit_result_release (result);
  return 0;
}
