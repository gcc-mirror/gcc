#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

/* Quote from here in docs/topics/types.rst.  */

union int_or_float
{
  int as_int;
  float as_float;
};

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     float
     test_union (int i)
     {
        union int_or_float u;
	u.as_int = i;
	return u.as_float;
     }
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *float_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_FLOAT);
  gcc_jit_field *as_int =
    gcc_jit_context_new_field (ctxt,
                               NULL,
                               int_type,
                               "as_int");
  gcc_jit_field *as_float =
    gcc_jit_context_new_field (ctxt,
                               NULL,
                               float_type,
                               "as_float");
  gcc_jit_field *fields[] = {as_int, as_float};
  gcc_jit_type *union_type =
    gcc_jit_context_new_union_type (ctxt, NULL,
				    "int_or_float", 2, fields);

  /* Build the test function.  */
  gcc_jit_param *param_i =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "i");
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  float_type,
                                  "test_union",
                                  1, &param_i,
                                  0);

  gcc_jit_lvalue *u =
    gcc_jit_function_new_local (test_fn, NULL,
				union_type, "u");

  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);

  /* u.as_int = i; */
  gcc_jit_block_add_assignment (
    block,
    NULL,
    /* "u.as_int = ..." */
    gcc_jit_lvalue_access_field (u,
				 NULL,
				 as_int),
    gcc_jit_param_as_rvalue (param_i));

  /* return u.as_float; */
  gcc_jit_block_end_with_return (
    block, NULL,
    gcc_jit_rvalue_access_field (gcc_jit_lvalue_as_rvalue (u),
				 NULL,
				 as_float));
}

/* Quote up to here in docs/topics/types.rst.  */

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef float (*fn_type) (int i);
  CHECK_NON_NULL (result);

  fn_type test_union =
    (fn_type)gcc_jit_result_get_code (result, "test_union");
  CHECK_NON_NULL (test_union);

  /* Call the JIT-generated function.  */
  float f_result = test_union (42);

  union int_or_float u;
  u.as_float = f_result;

  CHECK_VALUE (u.as_int, 42);
}
