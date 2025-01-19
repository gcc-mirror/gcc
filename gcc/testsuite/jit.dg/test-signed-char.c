#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
        int test_signed_char ()
        {
            char val = -2;
            return (int) val;
        }
    */
  gcc_jit_type *char_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CHAR);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "test_signed_char",
				  0, NULL,
				  0);

  gcc_jit_block *block = gcc_jit_function_new_block(test_fn, "entry");

  gcc_jit_rvalue *val = gcc_jit_context_new_rvalue_from_int (ctxt,
    char_type, -2);
  gcc_jit_rvalue *return_value = gcc_jit_context_new_cast (
    ctxt, NULL, val, int_type);

  gcc_jit_block_end_with_return (block, NULL, return_value);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);

  typedef int (*fn_type) ();
  fn_type test_signed_char =
    (fn_type)gcc_jit_result_get_code (result, "test_signed_char");
  CHECK_NON_NULL (test_signed_char);
  CHECK_VALUE (test_signed_char (), -2);
}
