#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

struct bar
{
  int x;
  int y;
};

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     int
     test_reading (const struct bar *f)
     {
       return f->x * f->y;
     }

     int
     test_writing ()
     {
       struct bar tmp;
       tmp.x = 5;
       tmp.y = 7;
       return test_reading (&tmp);
     }
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_field *x =
    gcc_jit_context_new_field (ctxt,
                               NULL,
                               int_type,
                               "x");
  gcc_jit_field *y =
    gcc_jit_context_new_field (ctxt,
                               NULL,
                               int_type,
                               "y");
  gcc_jit_field *fields[] = {x, y};
  gcc_jit_type *struct_type =
    gcc_jit_struct_as_type (
      gcc_jit_context_new_struct_type (ctxt, NULL, "bar", 2, fields));
  gcc_jit_type *const_struct_type = gcc_jit_type_get_const (struct_type);
  gcc_jit_type *ptr_type = gcc_jit_type_get_pointer (const_struct_type);

  /* Build "test_reading".  */
  gcc_jit_param *param_f =
    gcc_jit_context_new_param (ctxt, NULL, ptr_type, "f");
  gcc_jit_function *fn_test_reading =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  int_type,
                                  "test_reading",
                                  1, &param_f,
                                  0);

  /* return f->x * f->y; */
  gcc_jit_block *reading_block = gcc_jit_function_new_block (fn_test_reading, NULL);
  gcc_jit_block_end_with_return (
    reading_block,
    NULL,
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      GCC_JIT_BINARY_OP_MULT,
      int_type,
      gcc_jit_lvalue_as_rvalue (
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_param_as_rvalue (param_f),
	  NULL,
	  x)),
      gcc_jit_lvalue_as_rvalue (
	gcc_jit_rvalue_dereference_field (
	gcc_jit_param_as_rvalue (param_f),
	NULL,
	y))));

  /* Build "test_writing".  */
  gcc_jit_function *fn_test_writing =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  int_type,
                                  "test_writing",
                                  0, NULL,
                                  0);

  /* struct bar tmp; */
  gcc_jit_lvalue *local_tmp =
    gcc_jit_function_new_local (fn_test_writing, NULL,
				struct_type,
				"tmp");
  /* tmp.x = 5; */
  gcc_jit_block *writing_block = gcc_jit_function_new_block (fn_test_writing, NULL);
  gcc_jit_block_add_assignment (
    writing_block, NULL,
    gcc_jit_lvalue_access_field (local_tmp, NULL, x),
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 5));

  /* tmp.y = 7; */
  gcc_jit_block_add_assignment (
    writing_block, NULL,
    gcc_jit_lvalue_access_field (local_tmp, NULL, y),
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 7));

  /* return test_reading (&tmp); */
  gcc_jit_rvalue *arg = gcc_jit_lvalue_get_address (local_tmp, NULL);
  gcc_jit_block_end_with_return (
    writing_block,
    NULL,
    gcc_jit_context_new_call (
      ctxt, NULL,
      fn_test_reading,
      1, &arg));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*fn_type) (void);
  CHECK_NON_NULL (result);

  fn_type test_writing =
    (fn_type)gcc_jit_result_get_code (result, "test_writing");
  CHECK_NON_NULL (test_writing);

  /* Verify that the code correctly returns the product of the fields.  */
  CHECK_VALUE (test_writing (), 35);
}

