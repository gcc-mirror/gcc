#include <libgccjit.h>

#include "harness.h"

/* Try to create a "void" global; the API ought to complain.  */

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *void_type
    = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_context_new_global (ctxt, NULL, GCC_JIT_GLOBAL_EXPORTED,
			      void_type, "i");
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_context_new_global:"
		      " void type for global \"i\"");
}
