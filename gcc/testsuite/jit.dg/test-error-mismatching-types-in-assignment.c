#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     void
     test_fn ()
     {
        int i;
        i = "this is not an int";
     }

     and verify that the API complains about the mismatching types
     in the assignment.
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type,
                                  "test_fn",
                                  0, NULL,
                                  0);
  gcc_jit_lvalue *i =
    gcc_jit_function_new_local (
      test_fn, NULL, int_type, "i");

  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);

  gcc_jit_block_add_assignment (
    block, NULL,
    i, /* of type int */
    gcc_jit_context_new_string_literal (
      ctxt, "this is not an int"));
  gcc_jit_block_end_with_void_return (block, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_block_add_assignment:"
		      " mismatching types:"
		      " assignment to i (type: int)"
		      " from \"this is not an int\" (type: const char *)");
}

