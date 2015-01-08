#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Trigger an API error by passing bad data.  */
  gcc_jit_context_new_function (ctxt, NULL,
                                GCC_JIT_FUNCTION_EXPORTED,
                                NULL, /* error: this must be non-NULL */
                                "hello_world",
                                0, NULL,
                                0);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Ensure that the bad API usage prevents the API giving a bogus
     result back.  */
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_context_new_function: NULL return_type");
  CHECK_STRING_VALUE (gcc_jit_context_get_last_error (ctxt),
		      "gcc_jit_context_new_function: NULL return_type");
}

