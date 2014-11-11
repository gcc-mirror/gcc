#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     int
     test_fn ()
     {
       struct foo f;
       return (int)f;
     }

     and verify that the API complains about the bad cast.
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);


  gcc_jit_struct *struct_foo =
    gcc_jit_context_new_struct_type (ctxt, NULL, "foo",
				     0, NULL);

  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  int_type,
                                  "test_fn",
                                  0, NULL,
                                  0);
  gcc_jit_lvalue *f =
    gcc_jit_function_new_local (
      test_fn,
      NULL,
      gcc_jit_struct_as_type (struct_foo), "f");

  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);

  gcc_jit_block_end_with_return (
    block, NULL,
    gcc_jit_context_new_cast (ctxt, NULL,
			      gcc_jit_lvalue_as_rvalue (f),
			      int_type));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_context_new_cast:"
		      " cannot cast f from type: struct foo"
		      " to type: int");
}

