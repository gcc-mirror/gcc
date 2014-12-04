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
     extern void called_function ();

     void
     test_caller (int a)
     {
        called_function (a);
     }

     and verify that the API complains about the mismatching arg
     counts.
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Declare the imported function.  */
  gcc_jit_function *called_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_IMPORTED,
                                  void_type,
                                  "called_function",
                                  0, NULL,
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
  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);
  /* called_function (a);  */
  gcc_jit_rvalue *arg = gcc_jit_param_as_rvalue (param_a);
  gcc_jit_block_add_eval (
    block, NULL,
    gcc_jit_context_new_call (ctxt,
                              NULL,
                              called_fn,
                              1, &arg));
  /* the above has the wrong arg count.  */
  gcc_jit_block_end_with_void_return (block, NULL);
}

extern void
called_function (void)
{
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Ensure that mismatching arg count leads to the API giving a NULL
     result back.  */
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      ("gcc_jit_context_new_call:"
		       " too many arguments to function \"called_function\""
		       " (got 1 args, expected 0)"));
}

