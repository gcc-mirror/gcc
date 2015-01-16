#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Verify that we get an error (rather than a crash)
     if the client code reuses a gcc_jit_param * within
     a function.  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Create a param.  */
  gcc_jit_param *param =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "i");

  /* Try to use it twice when creating "fn".  */
  gcc_jit_param *params[2];
  params[0] = param;
  params[1] = param;

  gcc_jit_context_new_function (ctxt, NULL,
				GCC_JIT_FUNCTION_IMPORTED,
				void_type,
				"fn",
				2, params,
				0);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      ("gcc_jit_context_new_function:"
		       " parameter i (type: int)"
		       " is used more than once when creating function"
		       " fn"))
}

