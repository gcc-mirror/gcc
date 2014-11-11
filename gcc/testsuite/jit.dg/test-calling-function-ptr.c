#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     void
     test_calling_function_ptr (void (*fn_ptr) (int, int, int) fn_ptr,
                                int a)
     {
        fn_ptr (a * 3, a * 4, a * 5);
     }
  */

  int i;
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Build the function ptr type.  */
  gcc_jit_type *param_types[3];
  param_types[0] = int_type;
  param_types[1] = int_type;
  param_types[2] = int_type;

  gcc_jit_type *fn_ptr_type =
    gcc_jit_context_new_function_ptr_type (ctxt, NULL,
					   void_type,
					   3, param_types, 0);

  /* Ensure that function ptr types have sane debug strings.  */

  CHECK_STRING_VALUE (
    gcc_jit_object_get_debug_string (gcc_jit_type_as_object (fn_ptr_type)),
    "void (*) (int, int, int)");

  /* Build the test_fn.  */
  gcc_jit_param *param_fn_ptr =
    gcc_jit_context_new_param (ctxt, NULL, fn_ptr_type, "fn_ptr");
  gcc_jit_param *param_a =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "a");

  gcc_jit_param *params[2];
  params[0] = param_fn_ptr;
  params[1] = param_a;

  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type,
                                  "test_calling_function_ptr",
                                  2, params,
                                  0);
  /* "a * 3, a * 4, a * 5"  */
  gcc_jit_rvalue *args[3];
  for (i = 0; i < 3; i++)
    args[i] =
      gcc_jit_context_new_binary_op (
        ctxt, NULL,
        GCC_JIT_BINARY_OP_MULT,
        int_type,
        gcc_jit_param_as_rvalue (param_a),
        gcc_jit_context_new_rvalue_from_int (
          ctxt,
          int_type,
          (i + 3) ));
  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);
  gcc_jit_block_add_eval (
    block, NULL,
    gcc_jit_context_new_call_through_ptr (
      ctxt,
      NULL,
      gcc_jit_param_as_rvalue (param_fn_ptr),
      3, args));
  gcc_jit_block_end_with_void_return (block, NULL);
}

static int called_through_ptr_with[3];

static void
function_called_through_fn_ptr (int i, int j, int k)
{
  called_through_ptr_with[0] = i;
  called_through_ptr_with[1] = j;
  called_through_ptr_with[2] = k;
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef void (*fn_type) (void (*fn_ptr) (int, int, int),
			   int);
  CHECK_NON_NULL (result);

  fn_type test_caller =
    (fn_type)gcc_jit_result_get_code (result, "test_calling_function_ptr");
  CHECK_NON_NULL (test_caller);

  called_through_ptr_with[0] = 0;
  called_through_ptr_with[1] = 0;
  called_through_ptr_with[2] = 0;

  /* Call the JIT-generated function.  */
  test_caller (function_called_through_fn_ptr, 5);

  /* Verify that it correctly called "function_called_through_fn_ptr".  */
  CHECK_VALUE (called_through_ptr_with[0], 15);
  CHECK_VALUE (called_through_ptr_with[1], 20);
  CHECK_VALUE (called_through_ptr_with[2], 25);
}

