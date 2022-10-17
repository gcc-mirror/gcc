/*

  Test that the proper error is triggered when we initialize
  a global array with a ctor with too many values.

  Using gcc_jit_global_set_initializer_rvalue()

*/

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{ /* float foo[1] = {1,2}; */

  gcc_jit_type *float_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_FLOAT);
  gcc_jit_type *arr_type = gcc_jit_context_new_array_type (ctxt,
							   0,
							   float_type,
							   1);
  gcc_jit_rvalue *rval_1 = gcc_jit_context_new_rvalue_from_int (
    ctxt, float_type, 1);
  gcc_jit_rvalue *rval_2 = gcc_jit_context_new_rvalue_from_int (
    ctxt, float_type, 2);

  gcc_jit_rvalue *values[] = {rval_1, rval_2};

  gcc_jit_rvalue *ctor = gcc_jit_context_new_array_constructor (ctxt,
								0,
								arr_type,
								2,
								values);
  if (!ctor)
    return;

  gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
    ctxt, NULL,
    GCC_JIT_GLOBAL_EXPORTED,
    arr_type,
    "global_floatarr_12");
  gcc_jit_global_set_initializer_rvalue (foo, ctor);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Ensure that the bad API usage prevents the API giving a bogus
     result back.  */
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted. */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_context_new_array_constructor: array "
		      "constructor has more values than the array type's "
		      "length (array type length: 1, constructor length: 2)");
  CHECK_STRING_VALUE (gcc_jit_context_get_last_error (ctxt),
		      "gcc_jit_context_new_array_constructor: array "
		      "constructor has more values than the array type's "
		      "length (array type length: 1, constructor length: 2)");
}
