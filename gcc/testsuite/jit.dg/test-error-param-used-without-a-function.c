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
       return i;
     }

     where "i" is a param that isn't associated with any function,
     and verify that the API complains.  */

  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_param *param =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "i");

  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  int_type,
                                  "test_fn",
                                  0, NULL,
                                  0);

  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);
  /* "return i;", using param i from the wrong function.  */
  gcc_jit_block_end_with_return (block,
				 NULL,
				 gcc_jit_param_as_rvalue (param));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      ("gcc_jit_block_end_with_return:"
		       " param i (type: int)"
		       " was used within function test_fn"
		       " (in statement: return i;)"
		       " but is not associated with any function"))
}

