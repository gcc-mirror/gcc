#include <libgccjit.h>

#include "harness.h"

/* Try to create a "void" local; the API ought to complain.  */

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
    gcc_jit_type *void_type
      = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
    gcc_jit_function *func
      = gcc_jit_context_new_function (ctxt, NULL, GCC_JIT_FUNCTION_EXPORTED,
				      void_type, "test_fn", 0, NULL, 0);
    gcc_jit_function_new_local(func, NULL, void_type, "i");
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_function_new_local:"
		      " void type for local \"i\"");
}
