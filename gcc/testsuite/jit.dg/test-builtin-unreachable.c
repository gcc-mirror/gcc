#include <libgccjit.h>

#include "harness.h"

/* Verify that we can compile a function that uses __builtin_unreachable
   (PR jit/95426).
   
   Compile the equivalent of this C:

     int test_pr95426_unreachable (int i)
     {
       __builtin_unreachable ();
       return i;
     }
*/

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_context_set_int_option (ctxt,
				  GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL,
				  0);

  gcc_jit_type *int_type = gcc_jit_context_get_type(ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_param *param_i
    = gcc_jit_context_new_param (ctxt, NULL, int_type, "i");
  gcc_jit_function *test_fn
    = gcc_jit_context_new_function (ctxt, NULL, GCC_JIT_FUNCTION_EXPORTED,
				    int_type,
				    "test_pr95426_unreachable",
				    1, &param_i, 0);

  gcc_jit_block *bb = gcc_jit_function_new_block(test_fn, "start");
  gcc_jit_function *func___builtin_unreachable
    = gcc_jit_context_get_builtin_function(ctxt, "__builtin_unreachable");
  gcc_jit_rvalue *call___builtin_unreachable
    = gcc_jit_context_new_call(ctxt, NULL, func___builtin_unreachable,
			       0, NULL);
  gcc_jit_block_add_eval(bb, NULL, call___builtin_unreachable);
  gcc_jit_block_end_with_return (bb, NULL, gcc_jit_param_as_rvalue(param_i));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);
  CHECK_NON_NULL (gcc_jit_result_get_code (result, "test_pr95426_unreachable"));
  /* Don't actually run the code.  */
}
