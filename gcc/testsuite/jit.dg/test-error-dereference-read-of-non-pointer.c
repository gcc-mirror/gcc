#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
       void
       int test_bogus_dereference_read (int i)
       {
	 return *i;
       }
     i.e. where i is *not* a pointer.
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Build the test function.  */
  gcc_jit_param *param_i =
      gcc_jit_context_new_param (ctxt, NULL, int_type, "i");
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type,
                                  "test_bogus_dereference_read",
                                  1, &param_i,
                                  0);
  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);
  /* Erroneous: "return *i;" */
  gcc_jit_block_end_with_return (
    block,
    NULL,
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_rvalue_dereference (
        gcc_jit_param_as_rvalue (param_i),
	NULL)));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      ("gcc_jit_rvalue_dereference:"
		       " dereference of non-pointer i (type: int)"));
}

