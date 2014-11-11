#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

#ifdef __cplusplus
extern "C" {
#endif

  extern int the_global;

#ifdef __cplusplus
}
#endif

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     extern int the_global;

     void
     test_using_global (void)
     {
	the_global += 1;
     }
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Build the test_fn.  */
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  void_type,
				  "test_using_global",
				  0, NULL,
				  0);
  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);

  gcc_jit_block_add_assignment_op (
    block, NULL,
    gcc_jit_context_new_global (ctxt, NULL, int_type, "the_global"),
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_one (ctxt, int_type));
  gcc_jit_block_end_with_void_return (block, NULL);
}

int the_global;

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef void (*fn_type) (void);
  CHECK_NON_NULL (result);

  fn_type test_using_global =
    (fn_type)gcc_jit_result_get_code (result, "test_using_global");
  CHECK_NON_NULL (test_using_global);

  the_global = 42;

  /* Call the JIT-generated function.  */
  test_using_global ();

  /* Verify that it correctly modified the_global.  */
  CHECK_VALUE (the_global, 43);
}

