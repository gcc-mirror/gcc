#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

/* Try to create a local of an opaque struct;
   the API ought to complain.  */

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *t_void =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);

  gcc_jit_struct *t_opaque =
    gcc_jit_context_new_opaque_struct (ctxt, NULL, "opaque");

  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  t_void,
                                  "test_fn",
				  0, NULL,
                                  0);

  (void)gcc_jit_function_new_local (func, NULL,
				    gcc_jit_struct_as_type (t_opaque),
				    "i");
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_function_new_local:"
		      " unknown size for local \"i\" (type: struct opaque)");
}
