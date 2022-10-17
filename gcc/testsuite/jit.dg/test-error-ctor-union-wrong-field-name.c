/*

  Test that the proper error is triggered when we build a ctor
  for an union type, but don't provide a correct field.

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
  gcc_jit_type *double_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_DOUBLE);

  gcc_jit_field *b1 = gcc_jit_context_new_field (ctxt,
						 0,
						 int_type,
						 "a");
  gcc_jit_field *b2 = gcc_jit_context_new_field (ctxt,
						 0,
						 float_type,
						 "b");
  gcc_jit_field *b3 = gcc_jit_context_new_field (ctxt,
						 0,
						 double_type,
						 "c");
  gcc_jit_field *fields_b[] = {b1, b2, b3};

  gcc_jit_type *union_bar_type =
      gcc_jit_context_new_union_type (ctxt,
				      0,
				      "bar",
				      3,
				      fields_b);

  gcc_jit_field *b33 = gcc_jit_context_new_field (ctxt,
						  0,
						  double_type,
						  "c");

  gcc_jit_rvalue *val =
    gcc_jit_context_new_rvalue_from_double (ctxt, double_type, 1);

  gcc_jit_rvalue *ctor = gcc_jit_context_new_union_constructor
    (ctxt, 0,
     union_bar_type,
     b33,
     val);

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
		      "gcc_jit_context_new_union_constructor: field object (c)"
		      " was not used when creating the type union bar");
  CHECK_STRING_VALUE (gcc_jit_context_get_last_error (ctxt),
		      "gcc_jit_context_new_union_constructor: field object (c)"
		      " was not used when creating the type union bar");
}
