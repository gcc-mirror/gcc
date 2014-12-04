#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
       int
       test_fn ()
       {
       }
     and verify that the API complains about the lack of
     a returned value.
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  (void)gcc_jit_context_new_function (ctxt, NULL,
				      GCC_JIT_FUNCTION_EXPORTED,
				      int_type,
				      "test_fn",
				      0, NULL,
				      0);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "function test_fn returns non-void (type: int)"
		      " but has no blocks");
}

