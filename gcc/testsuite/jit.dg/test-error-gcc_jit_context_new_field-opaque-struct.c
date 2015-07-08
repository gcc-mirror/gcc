#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

/* Try to put an opaque struct inside another struct
   (or union); the API ought to complain.  */

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_struct *t_opaque =
    gcc_jit_context_new_opaque_struct (ctxt, NULL, "opaque");

  (void)gcc_jit_context_new_field (ctxt, NULL,
				   gcc_jit_struct_as_type (t_opaque),
				   "f_opaque");
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_context_new_field:"
		      " unknown size for field \"f_opaque\" (type: struct opaque)");
}
