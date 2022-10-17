#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
char
my_casts (int x)
{
   return (char)(long) x;
}
   */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *long_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_LONG);
  gcc_jit_type *return_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CHAR);

  gcc_jit_param *x =
    gcc_jit_context_new_param (
      ctxt,
      NULL,
      int_type, "x");
  gcc_jit_param *params[1] = {x};
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt,
				  NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  return_type,
				  "my_casts",
				  1, params, 0);

  gcc_jit_block *initial =
    gcc_jit_function_new_block (func, "initial");

  gcc_jit_block_end_with_return(initial, NULL,
    gcc_jit_context_new_cast(ctxt,
        NULL,
        gcc_jit_context_new_cast(ctxt,
            NULL,
            gcc_jit_param_as_rvalue(x),
            long_type
        ),
        return_type
    ));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*my_casts_fn_type) (int);
  CHECK_NON_NULL (result);
  my_casts_fn_type my_casts =
    (my_casts_fn_type)gcc_jit_result_get_code (result, "my_casts");
  CHECK_NON_NULL (my_casts);
  char val = my_casts (10);
  note ("my_casts returned: %d", val);
  CHECK_VALUE (val, 10);
}
