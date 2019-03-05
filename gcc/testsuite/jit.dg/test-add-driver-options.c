#include <stdio.h>
#include <stdlib.h>

#include "libgccjit.h"
#include "harness.h"

#ifndef LIBGCCJIT_HAVE_gcc_jit_context_add_driver_option
#error LIBGCCJIT_HAVE_gcc_jit_context_add_driver_option was not defined
#endif

void
create_code (gcc_jit_context *ctxt, void *user_data)
{

  gcc_jit_context_add_driver_option (ctxt, "-L./");
  gcc_jit_context_add_driver_option (ctxt, "-ladd-driver-options-testlib");

  /* Let's try to inject the equivalent of:

      int caller_function (void)
      {
        return callee_function ();
      }
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_function *caller_func =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  int_type,
                                  "caller_function",
                                  0, NULL,
                                  0);

  gcc_jit_block *block =
    gcc_jit_function_new_block (caller_func, NULL);

  gcc_jit_function *callee_func =
    gcc_jit_context_new_function(ctxt, NULL,
				 GCC_JIT_FUNCTION_IMPORTED,
				 int_type,
				 "callee_function",
				 0, NULL,
				 1);

  gcc_jit_block_end_with_return (block, NULL,
				 gcc_jit_context_new_call(ctxt,
							  NULL,
							  callee_func,
							  0,
							  0));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*my_caller_fn_type) (void);

  CHECK_NON_NULL (result);
  my_caller_fn_type callee_function_ptr =
    (my_caller_fn_type)gcc_jit_result_get_code (result, "callee_function");
  CHECK_NON_NULL (callee_function_ptr);

  int res = callee_function_ptr ();

  CHECK_VALUE (res, 1978);
}
