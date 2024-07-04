#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
int
my_sizeof ()
{
   return sizeof(int32_t);
}
   */
  gcc_jit_type *int32 =
    gcc_jit_context_get_int_type (ctxt, 4, 1);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt,
				  NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "my_sizeof",
				  0, NULL, 0);

  gcc_jit_block *initial =
    gcc_jit_function_new_block (func, "initial");

  gcc_jit_block_end_with_return(initial, NULL,
    gcc_jit_context_new_sizeof(ctxt, int32));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*my_sizeof_type) ();
  CHECK_NON_NULL (result);
  my_sizeof_type my_sizeof =
    (my_sizeof_type)gcc_jit_result_get_code (result, "my_sizeof");
  CHECK_NON_NULL (my_sizeof);
  int val = my_sizeof ();
  CHECK_VALUE (val, 4);
}
