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
        return 42;
     }

     and verify that the API complains about the return
     of a value within a function with "void" return.
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type, /* "void" return */
                                  "test_fn",
                                  0, NULL,
                                  0);
  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);

  /* "return 42;"  (i.e. non-void) */
  gcc_jit_block_end_with_return (
    block, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 42));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Ensure that the "return 42" leads to the API giving a NULL
     result back.  */
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_block_end_with_return:"
		      " mismatching types: return of (int)42 (type: int)"
		      " in function test_fn (return type: void)");
}

