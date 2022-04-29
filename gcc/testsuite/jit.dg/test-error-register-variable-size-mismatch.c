/*

  Test that the proper error is triggered when we build a register variable
  with a register name that doesn't exist.

*/

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *array_type =
    gcc_jit_context_new_array_type (ctxt, NULL, int_type, 4096);
  gcc_jit_lvalue *global_variable =
    gcc_jit_context_new_global (
      ctxt, NULL, GCC_JIT_GLOBAL_EXPORTED, array_type, "global_variable");
  gcc_jit_lvalue_set_register_name(global_variable, "r12");
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Ensure that the bad API usage prevents the API giving a bogus
     result back.  */
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted. */
  CHECK_STRING_VALUE
    (gcc_jit_context_get_last_error (ctxt),
     "data type of 'global_variable' isn't suitable for a register");
}
