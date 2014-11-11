#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

#ifdef __cplusplus
extern "C" {
#endif

  extern void
  called_function (void *ptr);

#ifdef __cplusplus
}
#endif

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     extern void called_function (void *ptr);

     void
     test_fn ()
     {
        called_function (42);
     }

     and verify that the API complains about the mismatching argument
     type ("int" vs "void *").  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *void_ptr_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID_PTR);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Declare the imported function.  */
  gcc_jit_param *param =
    gcc_jit_context_new_param (ctxt, NULL, void_ptr_type, "ptr");
  gcc_jit_function *called_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_IMPORTED,
                                  void_type,
                                  "called_function",
                                  1, &param,
                                  0);

  /* Build the test_fn.  */
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type,
                                  "test_fn",
                                  0, NULL,
                                  0);
  /* called_function (42);  */
  gcc_jit_rvalue *arg =
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 42);

  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);
  gcc_jit_block_add_eval (
    block, NULL,
    gcc_jit_context_new_call (ctxt,
                              NULL,
                              called_fn,
                              1, &arg));
  /* the above has the wrong type for argument 1.  */
  gcc_jit_block_end_with_void_return (block, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      ("gcc_jit_context_new_call:"
		       " mismatching types for argument 1"
		       " of function \"called_function\":"
		       " assignment to param ptr (type: void *)"
		       " from (int)42 (type: int)"));
}

