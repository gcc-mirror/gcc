/*

  Test that the proper error is triggered when we build a ctor
  for an array type, but has the type wrong on an element.

*/

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *int_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_INT);
  gcc_jit_type *float_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_FLOAT);

  gcc_jit_type *arr_type =
    gcc_jit_context_new_array_type (ctxt, 0, int_type, 10);

  gcc_jit_rvalue *frv = gcc_jit_context_new_rvalue_from_double (ctxt,
								float_type,
								12);

  gcc_jit_rvalue *ctor = gcc_jit_context_new_array_constructor
    (ctxt, 0,
     arr_type,
     1,
     &frv);

  CHECK_VALUE (ctor, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Ensure that the bad API usage prevents the API giving a bogus
     result back.  */
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted. */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_context_new_array_constructor: array element "
		      "value types differ from types in 'values' (element "
		      "type: int)('values' type: float)");
  CHECK_STRING_VALUE (gcc_jit_context_get_last_error (ctxt),
		      "gcc_jit_context_new_array_constructor: array element "
		      "value types differ from types in 'values' (element "
		      "type: int)('values' type: float)");
}
