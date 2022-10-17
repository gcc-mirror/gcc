/*

  Test that the proper error is triggered when we initialize
  a global with a function call.

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

  gcc_jit_function *fn_int_3;
  { /* int foo () { int local = 3; return local;} */
    fn_int_3 =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    int_type,
				    "fn_int_3",
				    0,
				    0,
				    0);
    gcc_jit_block *block = gcc_jit_function_new_block (fn_int_3, "start");
    gcc_jit_lvalue *local = gcc_jit_function_new_local (fn_int_3,
							0,
							int_type,
							"local");
    gcc_jit_rvalue *rval = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);

    gcc_jit_block_add_assignment (block, 0, local, rval);

    gcc_jit_block_end_with_return (block,
				   0,
				   gcc_jit_lvalue_as_rvalue(local));

  }

  { /* int bar = foo(); */
    gcc_jit_rvalue *rval =
      gcc_jit_context_new_call (ctxt,
				0,
				fn_int_3,
				0,0);

    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      int_type,
      "global_nonconst_int");
    gcc_jit_global_set_initializer_rvalue (foo,
      rval);
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
		      " global_nonconst_int to a compile-time constant");
  CHECK_STRING_VALUE (gcc_jit_context_get_last_error (ctxt),
		      "unable to convert initial value for the global variable"
		      " global_nonconst_int to a compile-time constant");
}
