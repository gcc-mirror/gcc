/*

  Test that the proper error is triggered when we initialize
  a global with another non-const global's rvalue.

  Using gcc_jit_global_set_initializer_rvalue()

*/

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *int_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_INT);

  gcc_jit_lvalue *foo;
  { /* int bar; */
    foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      int_type,
      "global_lvalueinit_int1");
    gcc_jit_rvalue *rval = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_global_set_initializer_rvalue (foo,
      rval);
  }
  { /* int foo = bar; */

    gcc_jit_lvalue *bar =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      int_type,
      "global_lvalueinit_int2");
    gcc_jit_global_set_initializer_rvalue (bar,
					   gcc_jit_lvalue_as_rvalue (foo));
  }
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Ensure that the bad API usage prevents the API giving a bogus
     result back.  */
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted. */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "unable to convert initial value for the global variable"
		      " global_lvalueinit_int2 to a compile-time constant");
  CHECK_STRING_VALUE (gcc_jit_context_get_last_error (ctxt),
		      "unable to convert initial value for the global variable"
		      " global_lvalueinit_int2 to a compile-time constant");
}
