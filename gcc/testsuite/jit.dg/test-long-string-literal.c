#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "libgccjit.h"

#include "harness.h"

const char very_long_string[] =
  "abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc"
  "abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc"
  "abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc"
  "abcabcabcabcabcabcabcabcabcabca";

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Build the test_fn.  */
  gcc_jit_function *f =
    gcc_jit_context_new_function (
      ctxt, NULL,
      GCC_JIT_FUNCTION_EXPORTED,
      gcc_jit_context_get_type(ctxt,
			       GCC_JIT_TYPE_CONST_CHAR_PTR),
				"test_long_string_literal",
				0, NULL, 0);
  gcc_jit_block *blk =
    gcc_jit_function_new_block (f, "init_block");

  /* very_long_string is longer than 200 characters to specifically
     check that the previous limitation no longer apply.  */

  assert (sizeof (very_long_string) > 200);
  gcc_jit_rvalue *res =
    gcc_jit_context_new_string_literal (ctxt, very_long_string);

  gcc_jit_block_end_with_return (blk, NULL, res);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef const char *(*fn_type) (void);
  CHECK_NON_NULL (result);
  fn_type test_long_string_literal =
    (fn_type)gcc_jit_result_get_code (result, "test_long_string_literal");
  CHECK_NON_NULL (test_long_string_literal);

  /* Call the JIT-generated function.  */
  const char *str = test_long_string_literal ();
  CHECK_NON_NULL (str);
  CHECK_VALUE (strcmp (str, very_long_string), 0);
}
