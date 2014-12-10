#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to access an array at an index that isn't of a numeric
     type and verify that the API complains about the bad type.
  */
  gcc_jit_rvalue *string =
    gcc_jit_context_new_string_literal (ctxt,
					"hello world");

  (void)gcc_jit_context_new_array_access (
    ctxt, NULL,
    string, /* ptr */
    string /* index, not of numeric type */);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_context_new_array_access:"
		      " index: \"hello world\" (type: const char *)"
		      " is not of numeric type");
}
