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
        const char *variable;
        variable += "test";
     }

     and verify that the API complains about the mismatching types
     in the assignments.
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *const_char_ptr_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CONST_CHAR_PTR);

  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type,
                                  "test_fn",
                                  0, NULL,
                                  0);

  gcc_jit_lvalue *variable = gcc_jit_function_new_local (func, NULL, const_char_ptr_type, "variable");
  gcc_jit_block *initial =
    gcc_jit_function_new_block (func, "initial");
  gcc_jit_rvalue *string =
    gcc_jit_context_new_string_literal (ctxt, "test");
  gcc_jit_block_add_assignment_op (initial, NULL, variable, GCC_JIT_BINARY_OP_PLUS, string);

  gcc_jit_block_end_with_void_return (initial, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error messages were emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_block_add_assignment_op:"
                      " gcc_jit_block_add_assignment_op GCC_JIT_BINARY_OP_PLUS"
                      " has non-numeric lvalue variable (type: const char *)");
}

