#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Do nothing.  */
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Get the built-in functions.  */
  gcc_jit_function *builtin_sin =
    gcc_jit_context_get_builtin_function (ctxt, "sin");

  CHECK_VALUE (gcc_jit_function_get_param_count(builtin_sin), 1);

  gcc_jit_type *double_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_DOUBLE);
  CHECK_VALUE (gcc_jit_function_get_return_type(builtin_sin), double_type);
  CHECK (!gcc_jit_type_is_integral(double_type));

  gcc_jit_type *bool_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_BOOL);
  CHECK (gcc_jit_type_is_bool(bool_type));
  CHECK (!gcc_jit_type_is_integral(bool_type));

  gcc_jit_type *aligned_bool_type =
    gcc_jit_type_get_aligned(gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_BOOL), 8);
  CHECK (gcc_jit_type_is_bool(aligned_bool_type));
  CHECK (bool_type != aligned_bool_type);
  CHECK_VALUE (gcc_jit_type_unqualified(aligned_bool_type), bool_type);

  CHECK_VALUE (gcc_jit_type_unqualified(gcc_jit_type_get_const(bool_type)), bool_type);
  CHECK_VALUE (gcc_jit_type_unqualified(gcc_jit_type_get_volatile(bool_type)), bool_type);

  gcc_jit_type *int64 =
    gcc_jit_context_get_int_type(ctxt, 8, 1);
  CHECK (gcc_jit_type_is_integral(int64));
  gcc_jit_type *uint64 =
    gcc_jit_context_get_int_type(ctxt, 8, 0);
  CHECK (gcc_jit_type_is_integral(uint64));
  gcc_jit_type *int8 =
    gcc_jit_context_get_int_type(ctxt, 1, 1);
  CHECK (gcc_jit_type_is_integral(int8));
  gcc_jit_type *uint8 =
    gcc_jit_context_get_int_type(ctxt, 1, 0);
  CHECK (gcc_jit_type_is_integral(uint8));

  CHECK (!gcc_jit_type_dyncast_vector(double_type));
  gcc_jit_type *vec_type = gcc_jit_type_get_vector (double_type, 4);
  gcc_jit_vector_type *vector_type = gcc_jit_type_dyncast_vector(vec_type);
  CHECK (vector_type);
  CHECK (vec_type != double_type);
  CHECK_VALUE (gcc_jit_vector_type_get_element_type(vector_type), double_type);
  CHECK_VALUE (gcc_jit_vector_type_get_num_units(vector_type), 4);
  CHECK (!gcc_jit_type_is_integral(vec_type));

  CHECK (!gcc_jit_type_is_pointer(double_type));
  CHECK_VALUE (gcc_jit_type_is_pointer(gcc_jit_type_get_pointer(double_type)), double_type);

  gcc_jit_type* params[2] = {int8, uint64};
  gcc_jit_type *function_ptr_type = gcc_jit_context_new_function_ptr_type(ctxt, NULL, int64, 2, params, 0);
  CHECK (!gcc_jit_type_dyncast_function_ptr_type (int64));
  gcc_jit_function_type *function_type = gcc_jit_type_dyncast_function_ptr_type (function_ptr_type);
  CHECK (function_type);
  int param_count = gcc_jit_function_type_get_param_count(function_type);
  CHECK_VALUE (param_count, 2);
  gcc_jit_type *return_type = gcc_jit_function_type_get_return_type(function_type);
  CHECK_VALUE (return_type, int64);
  gcc_jit_type *param1 = gcc_jit_function_type_get_param_type(function_type, 0);
  CHECK_VALUE (param1, int8);
  gcc_jit_type *param2 = gcc_jit_function_type_get_param_type(function_type, 1);
  CHECK_VALUE (param2, uint64);

  gcc_jit_field *field1 = gcc_jit_context_new_field (ctxt, NULL, uint64, "field1");
  gcc_jit_field *field2 = gcc_jit_context_new_field (ctxt, NULL, double_type, "field2");
  gcc_jit_field *fields[2] = { field1, field2 };
  gcc_jit_struct *struct_type = gcc_jit_context_new_struct_type (ctxt, NULL, "testStruct", 2, fields);
  CHECK_VALUE (gcc_jit_struct_get_field_count(struct_type), 2);
  CHECK_VALUE (gcc_jit_struct_get_field(struct_type, 0), field1);
  CHECK_VALUE (gcc_jit_struct_get_field(struct_type, 1), field2);
  CHECK (!gcc_jit_type_is_struct(double_type));
  gcc_jit_struct *struct_ty = gcc_jit_type_is_struct(gcc_jit_struct_as_type(struct_type));
  CHECK_VALUE (struct_ty, struct_type);

  CHECK (!gcc_jit_type_dyncast_array(double_type));
  gcc_jit_type* array_type = gcc_jit_context_new_array_type(ctxt, NULL, double_type, 1);
  CHECK_VALUE (gcc_jit_type_dyncast_array(array_type), double_type);
}

