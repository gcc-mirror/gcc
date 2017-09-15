#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_context_get_builtin_function (ctxt,
					"this_is_not_a_builtin");
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "builtin \"this_is_not_a_builtin\" not found");
}
