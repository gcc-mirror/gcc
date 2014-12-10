#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     void
     test_fn (void *some_ptr)
     {
        ((some_unspecified_fn_ptr_type)some_ptr) (42);
     }

     and verify that the API complains about the 42 not being a
     function pointer.  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *void_ptr_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID_PTR);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Build the test_fn.  */
  gcc_jit_param *some_ptr =
    gcc_jit_context_new_param (ctxt, NULL, void_ptr_type, "some_ptr");

  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type,
                                  "test_fn",
                                  1, &some_ptr,
                                  0);
  gcc_jit_rvalue *arg =
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 42);

  /* ((some_unspecified_fn_ptr_type)some_ptr) (42); */
  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);
  gcc_jit_block_add_eval (
    block, NULL,
    gcc_jit_context_new_call_through_ptr (
      ctxt,
      NULL,
      /* This is not a function pointer.  */
      gcc_jit_param_as_rvalue (some_ptr),
      1, &arg));
  gcc_jit_block_end_with_void_return (block, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      ("gcc_jit_context_new_call_through_ptr:"
		       " fn_ptr is not a function ptr: some_ptr type: void *"));
}

