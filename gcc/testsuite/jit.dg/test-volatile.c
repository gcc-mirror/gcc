#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

volatile int the_volatile;

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     extern volatile int the_volatile;

     int
     test_using_volatile (void)
     {
	return the_volatile;
     }
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *volatile_int_type =
    gcc_jit_type_get_volatile (int_type);

  /* Build the test_fn.  */
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "test_using_volatile",
				  0, NULL,
				  0);
  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);
  gcc_jit_lvalue *read_of_the_volatile =
    gcc_jit_rvalue_dereference (
      gcc_jit_context_new_rvalue_from_ptr (
        ctxt,
	gcc_jit_type_get_pointer (volatile_int_type),
	(void *)&the_volatile),
      NULL);

  gcc_jit_block_end_with_return (
    block, NULL,
    gcc_jit_lvalue_as_rvalue (read_of_the_volatile));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*fn_type) (void);
  CHECK_NON_NULL (result);

  fn_type test_using_volatile =
    (fn_type)gcc_jit_result_get_code (result, "test_using_volatile");
  CHECK_NON_NULL (test_using_volatile);

  the_volatile = 42;

  /* Call the JIT-generated function.  */
  test_using_volatile ();

  CHECK_VALUE (test_using_volatile (), 42);
}

