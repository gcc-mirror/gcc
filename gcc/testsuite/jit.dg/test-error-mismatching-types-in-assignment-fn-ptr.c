#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     typedef void (*fn_ptr_iii) (int, int, int);
     typedef void (*fn_ptr_ifi) (int, float, int);
     void
     test_fn (void)
     {
        fn_ptr_iii iii_ptr;
        fn_ptr_ifi ifi_ptr;

	iii_ptr = NULL;
	ifi_ptr = iii_ptr;
     }

     and verify that the API complains about the mismatching types
     in the second assignment (but not the first).
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *float_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_FLOAT);

  gcc_jit_type *iii_types[] = {int_type, int_type, int_type};
  gcc_jit_type *fn_ptr_type_iii
    = gcc_jit_context_new_function_ptr_type (ctxt, NULL,
					     void_type,
					     3, iii_types,
					     0);

  gcc_jit_type *ifi_types[] = {int_type, float_type, int_type};
  gcc_jit_type *fn_ptr_type_ifi
    = gcc_jit_context_new_function_ptr_type (ctxt, NULL,
					     void_type,
					     3, ifi_types,
					     0);

  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_type,
                                  "test_fn",
                                  0, NULL,
                                  0);
  gcc_jit_lvalue *iii_ptr =
    gcc_jit_function_new_local (
      test_fn, NULL, fn_ptr_type_iii, "iii_ptr");
  gcc_jit_lvalue *ifi_ptr =
    gcc_jit_function_new_local (
      test_fn, NULL, fn_ptr_type_ifi, "ifi_ptr");

  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);

  /* iii_ptr = NULL; */
  gcc_jit_block_add_assignment (
    block, NULL,
    iii_ptr,
    gcc_jit_context_null (ctxt, fn_ptr_type_iii));
  /* ifi_ptr = iii_ptr; */
  gcc_jit_block_add_assignment (
    block, NULL,
    ifi_ptr,
    gcc_jit_lvalue_as_rvalue (iii_ptr));
  gcc_jit_block_end_with_void_return (block, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_block_add_assignment:"
		      " mismatching types:"
		      " assignment to ifi_ptr"
		      " (type: void (*) (int, float, int))"
		      " from iii_ptr"
		      " (type: void (*) (int, int, int))");
}

