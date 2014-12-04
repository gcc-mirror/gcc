#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to build an rvalue from a double with a non-numeric type
     and verify that the API complains about the bad type.
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);

  (void)gcc_jit_context_new_rvalue_from_double (ctxt, void_type, 42.0);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_context_new_rvalue_from_double:"
		      " not a numeric type: void");
}
