#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     const long integer = 10;

     void
     test_fn ()
     {
        integer = 12;
     }

     and verify that the API complains about assigning to a read-only
     variable.
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *long_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_LONG);

  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type,
                                  "test_fn",
                                  0, NULL,
                                  0);
  gcc_jit_block *initial =
    gcc_jit_function_new_block (func, "initial");

  gcc_jit_lvalue *integer =
    gcc_jit_context_new_global (
      ctxt, NULL, GCC_JIT_GLOBAL_EXPORTED, long_type, "integer");

  gcc_jit_rvalue *ten = gcc_jit_context_new_rvalue_from_int (ctxt, long_type, 10);
  gcc_jit_global_set_initializer_rvalue (integer, ten);
  gcc_jit_global_set_readonly(integer);

  gcc_jit_rvalue *twelve = gcc_jit_context_new_rvalue_from_int (ctxt, long_type, 12);
  gcc_jit_block_add_assignment(initial, NULL, integer, twelve);

  gcc_jit_block_end_with_void_return (initial, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error messages were emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_block_add_assignment:"
		      " cannot assign to readonly variable: integer");
}
