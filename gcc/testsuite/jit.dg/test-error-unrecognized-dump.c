#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

static char *dump;

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_context_enable_dump (ctxt,
			       "not-a-valid-dump-switch",
			       &dump);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "unrecognized dump: not-a-valid-dump-switch");
}

