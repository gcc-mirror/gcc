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
	 return;

	 return;
       }
     where the second block is unreachable.
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
  gcc_jit_block *initial =
    gcc_jit_function_new_block (test_fn, "a");
  gcc_jit_block *unreachable =
    gcc_jit_function_new_block (test_fn, "b");

  gcc_jit_block_end_with_void_return (initial, NULL);

  gcc_jit_block_end_with_void_return (unreachable, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "unreachable block: b");
}
