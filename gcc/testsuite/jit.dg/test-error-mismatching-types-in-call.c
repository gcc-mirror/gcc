#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

       struct foo;

       extern void called_function (struct foo *ptr);

       void
       test_fn ()
       {
         struct foo f;
	 called_function (f);
     }

     and verify that we get a type error (foo * vs foo).
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_struct *struct_foo =
    gcc_jit_context_new_struct_type (ctxt, NULL, "foo", 0, NULL);
  gcc_jit_type *foo_ptr =
    gcc_jit_type_get_pointer (gcc_jit_struct_as_type (struct_foo));
  gcc_jit_param *param =
    gcc_jit_context_new_param (ctxt, NULL, foo_ptr, "ptr");

  gcc_jit_function *called_function =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_IMPORTED,
                                  void_type,
                                  "called_function",
                                  1, &param,
                                  0);

  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type,
                                  "test_fn",
                                  0, NULL,
                                  0);
  gcc_jit_lvalue *f =
    gcc_jit_function_new_local (
      test_fn, NULL, gcc_jit_struct_as_type (struct_foo), "f");

  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);

  gcc_jit_rvalue *arg = gcc_jit_lvalue_as_rvalue (f);

  gcc_jit_block_add_eval (
    block, NULL,
    gcc_jit_context_new_call (
      ctxt, NULL,
      called_function,
      1, &arg));
  gcc_jit_block_end_with_void_return (block, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_context_new_call:"
		      " mismatching types for argument 1"
		      " of function \"called_function\":"
		      " assignment to param ptr (type: struct foo *)"
		      " from f (type: struct foo)");
}

