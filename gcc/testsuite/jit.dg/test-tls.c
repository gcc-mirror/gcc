#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     _Thread_local int foo;

     int test_using_tls()
     {
      foo = 42;
      return foo;
     }
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_lvalue *foo =
    gcc_jit_context_new_global (
      ctxt, NULL, GCC_JIT_GLOBAL_EXPORTED, int_type, "foo");
  gcc_jit_lvalue_set_tls_model (foo, GCC_JIT_TLS_MODEL_GLOBAL_DYNAMIC);

  /* Build the test_fn.  */
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "test_using_tls",
				  0, NULL,
				  0);
  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);
  gcc_jit_block_add_assignment (
    block, NULL,
    foo,
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 42));
  gcc_jit_block_end_with_return (block,
				 NULL,
				 gcc_jit_lvalue_as_rvalue (foo));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*fn_type) (void);
  CHECK_NON_NULL (result);

  fn_type test_using_tls =
    (fn_type)gcc_jit_result_get_code (result, "test_using_tls");
  CHECK_NON_NULL (test_using_tls);
  int return_value = test_using_tls();
  CHECK_VALUE (return_value, 42);

  int *foo = (int *)gcc_jit_result_get_global (result, "foo");
  CHECK_NON_NULL (foo);
  CHECK_VALUE (*foo, 42);
}
