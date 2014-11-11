#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

#ifdef __cplusplus
extern "C" {
#endif

  extern void
  called_function (int i, int j, int k);

#ifdef __cplusplus
}
#endif

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     extern void called_function (int i, int j, int k);

     void
     test_caller (int a)
     {
        called_function (a * 3, a * 4, a * 5);
     }
  */
  int i;
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Declare the imported function.  */
  gcc_jit_param *params[3];
  params[0] =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "i");
  params[1] =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "j");
  params[2] =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "k");
  gcc_jit_function *called_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_IMPORTED,
                                  void_type,
                                  "called_function",
                                  3, params,
                                  0);

  /* Build the test_fn.  */
  gcc_jit_param *param_a =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "a");
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type,
                                  "test_caller",
                                  1, &param_a,
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
    gcc_jit_context_new_call (ctxt,
                              NULL,
                              called_fn,
                              3, args));
  gcc_jit_block_end_with_void_return (block, NULL);
}

static int called_with[3];

extern void
called_function (int i, int j, int k)
{
  called_with[0] = i;
  called_with[1] = j;
  called_with[2] = k;
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef void (*fn_type) (int);
  CHECK_NON_NULL (result);

  fn_type test_caller =
    (fn_type)gcc_jit_result_get_code (result, "test_caller");
  CHECK_NON_NULL (test_caller);

  called_with[0] = 0;
  called_with[1] = 0;
  called_with[2] = 0;

  /* Call the JIT-generated function.  */
  test_caller (5);

  /* Verify that it correctly called "called_function".  */
  CHECK_VALUE (called_with[0], 15);
  CHECK_VALUE (called_with[1], 20);
  CHECK_VALUE (called_with[2], 25);
}

