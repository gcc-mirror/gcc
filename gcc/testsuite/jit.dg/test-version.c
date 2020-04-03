#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

#ifndef LIBGCCJIT_HAVE_gcc_jit_version
#error LIBGCCJIT_HAVE_gcc_jit_version was not defined
#endif

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Do nothing.  */
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  if (!gcc_jit_version_major ())
    fail ("Major version is zero");
  /* Minor and patchlevel can be zero.  */
  gcc_jit_version_minor ();
  gcc_jit_version_patchlevel ();
}
