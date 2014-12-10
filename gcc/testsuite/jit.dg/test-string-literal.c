#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     const char *
     test_string_literal (void)
     {
        return "hello world";
     }
  */
  gcc_jit_type *const_char_ptr_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CONST_CHAR_PTR);

  /* Build the test_fn.  */
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  const_char_ptr_type,
                                  "test_string_literal",
                                  0, NULL,
                                  0);
  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);

  gcc_jit_block_end_with_return (
    block, NULL,
    gcc_jit_context_new_string_literal (ctxt, "hello world"));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef const char *(*fn_type) (void);
  CHECK_NON_NULL (result);
  fn_type test_string_literal =
    (fn_type)gcc_jit_result_get_code (result, "test_string_literal");
  CHECK_NON_NULL (test_string_literal);

  /* Call the JIT-generated function.  */
  const char *str = test_string_literal ();
  CHECK_NON_NULL (str);
  CHECK_VALUE (strcmp (str, "hello world"), 0);
}

