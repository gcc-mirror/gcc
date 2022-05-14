#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     int __attribute__ ((__vector_size__ (8))) convert_vec(double __attribute__ ((__vector_size__ (16))) double_vec)
     {
      return __builtin_convertvector (double_vec, int __attribute__ ((__vector_size__ (8))));
     }
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *int_vector_type =
    gcc_jit_type_get_vector (int_type, 2);
  gcc_jit_type *double_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_DOUBLE);
  gcc_jit_type *double_vector_type =
    gcc_jit_type_get_vector (double_type, 2);

  /* Build the convert_vec.  */
  gcc_jit_param *param =
    gcc_jit_context_new_param (ctxt, NULL, double_vector_type, "double_vec");
  gcc_jit_function *convert_vec =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_vector_type,
				  "convert_vec",
				  1, &param,
				  0);
  gcc_jit_block *block = gcc_jit_function_new_block (convert_vec, NULL);

  gcc_jit_lvalue *var = gcc_jit_function_new_local (convert_vec, NULL, double_vector_type, "mem_vec");
  gcc_jit_block_add_assignment (block, NULL, var, gcc_jit_param_as_rvalue (param));

  gcc_jit_block_end_with_return (block,
    NULL,
    gcc_jit_context_convert_vector (ctxt, NULL, gcc_jit_lvalue_as_rvalue (var), int_vector_type));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);

  typedef int __attribute__ ((__vector_size__ (8))) (*convert_vec)(double __attribute__ ((__vector_size__ (16))) double_vec);
  double __attribute__ ((__vector_size__ (16))) double_vec = { 3.2, 7.9 };
  convert_vec func = (convert_vec)gcc_jit_result_get_code (result, "convert_vec");
  int __attribute__ ((__vector_size__ (8))) res = func (double_vec);
  int __attribute__ ((__vector_size__ (8))) expected = { 3, 7 };
  CHECK_VECTOR_VALUE (2, res, expected);
}
