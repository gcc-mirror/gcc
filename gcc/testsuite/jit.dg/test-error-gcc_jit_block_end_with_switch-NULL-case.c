#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgccjit.h"

#include "harness.h"

/* Try to create a switch statement with a NULL case, so that
   we can verify that we get a sane error message.  */

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *t_int =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *return_type = t_int;
  gcc_jit_param *x =
    gcc_jit_context_new_param (ctxt, NULL, t_int, "x");
  gcc_jit_param *params[1] = {x};
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  return_type,
				  "test_switch",
				  1, params, 0);

  gcc_jit_block *b_initial =
    gcc_jit_function_new_block (func, "initial");

  gcc_jit_block *b_default =
    gcc_jit_function_new_block (func, "default");

  /* Erroneous NULL case.  */
  gcc_jit_case *cases[1] = {
    NULL
  };

  gcc_jit_block_end_with_switch (
    b_initial, NULL,
    gcc_jit_param_as_rvalue (x),
    b_default,
    1,
    cases);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_block_end_with_switch: NULL case 0");
}
