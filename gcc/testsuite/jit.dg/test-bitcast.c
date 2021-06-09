#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
int
my_bitcast (double x)
{
   return bitcast(x, int);
}
   */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *double_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_FLOAT);

  gcc_jit_param *x =
    gcc_jit_context_new_param (
      ctxt,
      NULL,
      double_type, "x");
  gcc_jit_param *params[1] = {x};
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt,
				  NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "my_bitcast",
				  1, params, 0);

  gcc_jit_block *initial =
    gcc_jit_function_new_block (func, "initial");

  gcc_jit_block_end_with_return(initial, NULL,
    gcc_jit_context_new_bitcast(ctxt,
        NULL,
        gcc_jit_param_as_rvalue(x),
        int_type
    ));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*my_bitcast_fn_type) (double);
  CHECK_NON_NULL (result);
  my_bitcast_fn_type my_bitcast =
    (my_bitcast_fn_type)gcc_jit_result_get_code (result, "my_bitcast");
  CHECK_NON_NULL (my_bitcast);
  int val = my_bitcast (-5.1298714);
  note ("my_bitcast returned: %d", val);
  CHECK_VALUE (val, 35569201);
}
