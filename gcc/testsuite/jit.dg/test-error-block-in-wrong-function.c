#include <math.h>
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
	 goto label;
       }

       void
       other_fn ()
       {
         label:
       };
     where the destination block is in another function.
  */
  gcc_jit_type *void_t =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);

  /* Build the test_fn.  */
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_t,
                                  "test_fn",
                                  0, NULL,
                                  0);
  /* Build the other_fn.  */
  gcc_jit_function *other_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_t,
                                  "other_fn",
                                  0, NULL,
                                  0);

  gcc_jit_block *initial =
    gcc_jit_function_new_block (test_fn, "initial");
  gcc_jit_block *block_within_other_fn =
    gcc_jit_function_new_block (other_fn, "block_within_other_fn");

  gcc_jit_block_end_with_jump (initial, NULL, block_within_other_fn);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_block_end_with_jump:"
		      " target block is not in same function:"
		      " source block initial is in function test_fn"
		      " whereas target block block_within_other_fn"
		      " is in function other_fn");
  /* Example of a testcase in which the last error != first error.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_last_error (ctxt),
		      "unterminated block in other_fn: block_within_other_fn");
}
