#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

/* Try to create an unary operator with invalid result type.  */

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *void_ptr_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID_PTR);

  gcc_jit_context_new_unary_op (
    ctxt,
    NULL,
    GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
    void_ptr_type,
    gcc_jit_context_new_rvalue_from_int (ctxt,
					 int_type,
					 1));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.	 */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_context_new_unary_op: gcc_jit_unary_op "
		      "GCC_JIT_UNARY_OP_LOGICAL_NEGATE with operand "
		      "(int)1 has non-numeric result_type: void *");
}
