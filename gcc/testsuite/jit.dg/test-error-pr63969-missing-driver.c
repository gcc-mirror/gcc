/* PR jit/63969: libgccjit would segfault inside gcc_jit_context_compile
   if the driver wasn't found on PATH if GCC_JIT_STR_OPTION_PROGNAME was
   NULL.  */

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Create nothing within the context.  */

  /* harness.h's set_options has set a sane value for
     GCC_JIT_STR_OPTION_PROGNAME, but PR jit/63969 only segfaulted if it's
     NULL.

     Unset it.  */
  gcc_jit_context_set_str_option (ctxt, GCC_JIT_STR_OPTION_PROGNAME, NULL);

  /* By default, we use an embedded copy of the driver.
     Opt-in to using an external copy of the driver.  */
  gcc_jit_context_set_bool_use_external_driver (ctxt, 1);

  /* Break PATH, so that the driver can't be found
     by gcc::jit::playback::context::compile ()
     within gcc_jit_context_compile.  */
  unsetenv ("PATH");
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that a sane error message was emitted.  */
  CHECK_STRING_STARTS_WITH (gcc_jit_context_get_first_error (ctxt),
			    "error invoking gcc driver");
}
