#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Do nothing.  */
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* We should have a non-NULL result, albeit one with nothing in it.  */
  CHECK_NON_NULL (result);
}

