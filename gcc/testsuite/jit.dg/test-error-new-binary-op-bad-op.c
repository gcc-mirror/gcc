#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Trigger an API error by passing bad data.  */
  (void)gcc_jit_context_new_binary_op (
	  ctxt,
	  NULL,

	  /* Non-valid enum value: */
	  (enum gcc_jit_binary_op) 42,

	  /* These aren't valid either: */
	  NULL, /* gcc_jit_type *result_type, */
	  NULL, NULL); /* gcc_jit_rvalue *a, gcc_jit_rvalue *b */

}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Ensure that the bad API usage prevents the API giving a bogus
     result back.  */
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      ("gcc_jit_context_new_binary_op:"
		       " unrecognized value for enum gcc_jit_binary_op: 42"));
}

