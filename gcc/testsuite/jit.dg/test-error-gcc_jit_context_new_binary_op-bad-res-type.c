#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

/* Try to create a binary operator with invalid result type.  */

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *void_ptr_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID_PTR);

  gcc_jit_context_new_binary_op (
    ctxt,
    NULL,
    GCC_JIT_BINARY_OP_MINUS,
    void_ptr_type,
    gcc_jit_context_new_rvalue_from_int (ctxt,
					 int_type,
					 1),
    gcc_jit_context_new_rvalue_from_int (ctxt,
					 int_type,
					 2));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.	 */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_context_new_binary_op: gcc_jit_binary_op "
		      "GCC_JIT_BINARY_OP_MINUS with operands a: "
		      "(int)1 b: (int)2 has non-numeric result_type: void *");
}
