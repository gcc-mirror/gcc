/*

  Test that the proper error is triggered when we initialize
  a global with a global that has no DECL_INITIAL (and is marked
  DECL_COMMON(NODE) = 1).

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

  /* const int foo;
     int bar = foo;
   */
  gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
    ctxt, NULL,
    GCC_JIT_GLOBAL_EXPORTED,
    gcc_jit_type_get_const (int_type),
    "global_const_int_0");
  gcc_jit_lvalue *bar =  gcc_jit_context_new_global (
    ctxt, NULL,
    GCC_JIT_GLOBAL_EXPORTED,
    int_type,
    "global_lvalueinit_int_0");
  gcc_jit_global_set_initializer_rvalue (bar,
					 gcc_jit_lvalue_as_rvalue (foo));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Ensure that the bad API usage prevents the API giving a bogus
     result back.  */
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted. */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "unable to convert initial value for the global "
		      "variable global_lvalueinit_int_0 to a compile-time"
		      " constant");
  CHECK_STRING_VALUE (gcc_jit_context_get_last_error (ctxt),
		      "unable to convert initial value for the global "
		      "variable global_lvalueinit_int_0 to a compile-time"
		      " constant");
}
