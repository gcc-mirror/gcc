#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

/* Try to dereference a bit-field.  */

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     struct bit_foo
     {
       int i:3;
       int j:3;
     };

     struct bit_foo f;
     &(f.j)
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_field *i =
    gcc_jit_context_new_bitfield (ctxt,
				  NULL,
				  int_type,
				  3,
				  "i");
  gcc_jit_field *j =
    gcc_jit_context_new_bitfield (ctxt,
				  NULL,
				  int_type,
				  3,
				  "j");
  gcc_jit_field *fields[] = {i, j};
  gcc_jit_type *struct_type =
    gcc_jit_struct_as_type (
      gcc_jit_context_new_struct_type (ctxt, NULL, "bit_foo", 2, fields));

  gcc_jit_lvalue *f_struct =
    gcc_jit_context_new_global (ctxt,
				NULL,
				GCC_JIT_GLOBAL_INTERNAL,
				struct_type,
				"f");

  gcc_jit_lvalue_get_address (
    gcc_jit_lvalue_access_field (
      f_struct,
      NULL,
      j),
    NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "cannot take address of bit-field");
}
