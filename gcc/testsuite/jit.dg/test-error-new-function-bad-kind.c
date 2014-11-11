#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Trigger an API error by passing bad data.  */
  (void)gcc_jit_context_new_function (
	  ctxt,
	  NULL,

	  /* Non-valid enum value: */
	  (enum gcc_jit_function_kind)42,

	  int_type, /* gcc_jit_type *return_type, */
	  "foo", /* const char *name, */
	  0, /* int num_params, */
	  NULL, /* gcc_jit_param **params, */
	  0); /* int is_variadic */
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Ensure that the bad API usage prevents the API giving a bogus
     result back.  */
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      ("gcc_jit_context_new_function:"
		       " unrecognized value for enum gcc_jit_function_kind: 42"));
}

