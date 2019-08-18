#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

/* Try to declare a bit-field with invalid width.  */

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *short_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_SHORT);
  gcc_jit_field *i =
    gcc_jit_context_new_bitfield (ctxt,
				  NULL,
				  short_type,
				  3,
				  "i");
  gcc_jit_field *j =
    gcc_jit_context_new_bitfield (ctxt,
				  NULL,
				  short_type,
				  157,
				  "j");
  gcc_jit_field *fields[] = {i, j};
  gcc_jit_context_new_struct_type (ctxt, NULL, "bit_foo", 2, fields);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);
  char error_str[256];
  snprintf (error_str, sizeof (error_str),
	    "width of bit-field j (width: 157) is wider than its type "
	    "(width: %zu)", 8 * sizeof (short));

  /* Verify that the correct error message was emitted.  */

  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      error_str);
}
