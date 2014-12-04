#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

#ifdef __cplusplus
extern "C" {
#endif

  extern void
  called_function (void);

#ifdef __cplusplus
}
#endif

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     void
     test_caller (void (*some_fn_ptr) (void), int a)
     {
        some_fn_ptr (a);
     }

     and verify that the API complains about the mismatching arg
     counts.
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Build the function ptr type.  */
  gcc_jit_type *fn_ptr_type =
    gcc_jit_context_new_function_ptr_type (ctxt, NULL,
					   void_type,
					   0, NULL, 0);

  /* Build the test_fn.  */
  gcc_jit_param *param_fn_ptr =
    gcc_jit_context_new_param (ctxt, NULL, fn_ptr_type, "some_fn_ptr");
  gcc_jit_param *param_a =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "a");
  gcc_jit_param *params[2];
  params[0] = param_fn_ptr;
  params[1] = param_a;

  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type,
                                  "test_caller",
                                  2, params,
                                  0);
  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);

  /* some_fn_ptr (a); */
  gcc_jit_rvalue *arg = gcc_jit_param_as_rvalue (param_a);
  gcc_jit_block_add_eval (
    block, NULL,
    gcc_jit_context_new_call_through_ptr (
      ctxt,
      NULL,
      gcc_jit_param_as_rvalue (param_fn_ptr),
      1, &arg));
  /* the above has too many args.  */
  gcc_jit_block_end_with_void_return (block, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Ensure that mismatching arg count leads to the API giving a NULL
     result back.  */
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_context_new_call_through_ptr:"
		      " too many arguments to fn_ptr:"
		      " some_fn_ptr (got 1 args, expected 0)");
}

