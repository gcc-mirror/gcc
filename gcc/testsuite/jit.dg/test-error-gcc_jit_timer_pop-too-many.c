#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_timer *timer = gcc_jit_timer_new ();
  /* Erroneous: pop of an empty timing stack.  */
  gcc_jit_timer_pop (timer, "test");
  gcc_jit_timer_release (timer);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* empty */
}
