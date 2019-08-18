#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

/* Try to declare a bit-field with invalid type.  */

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *bool_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_BOOL);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *long_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_LONG);
  gcc_jit_type *float_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_FLOAT);

  gcc_jit_context_new_bitfield (ctxt,
				NULL,
				bool_type,
				3,
				"b");
  gcc_jit_context_new_bitfield (ctxt,
				  NULL,
				  int_type,
				  3,
				  "i");
  gcc_jit_context_new_bitfield (ctxt,
				NULL,
				long_type,
				3,
				"l");
  gcc_jit_context_new_bitfield (ctxt,
				NULL,
				float_type,
				5,
				"f");
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.	 */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_context_new_bitfield: bit-field f has non "
		      "integral type float");
}
