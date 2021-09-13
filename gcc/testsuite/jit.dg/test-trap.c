#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>

#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     void
     test_trap (void)
     {
       *((int *)0) = 42;
     }
  */
  gcc_jit_type *void_type
    = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type
    = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *int_ptr_type
    = gcc_jit_type_get_pointer (int_type);

  /* Build the test_fn.  */
  gcc_jit_function *func
    = gcc_jit_context_new_function (ctxt, NULL,
				    GCC_JIT_FUNCTION_EXPORTED,
				    void_type,
				    "test_trap",
				    0, NULL,
				    0);

  gcc_jit_block *initial = gcc_jit_function_new_block (func, "initial");

  gcc_jit_rvalue *null_ptr
    = gcc_jit_context_new_rvalue_from_ptr (ctxt, int_ptr_type, NULL);

  /* "*((int *)0) = 42;" */
  gcc_jit_block_add_assignment (
    initial, NULL,
    gcc_jit_rvalue_dereference (null_ptr, NULL),
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 42));

  gcc_jit_block_end_with_void_return (initial, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef void (*fn_type) (void);
  CHECK_NON_NULL (result);
  fn_type test_array =
    (fn_type)gcc_jit_result_get_code (result, "test_trap");
  CHECK_NON_NULL (test_array);
  /* Don't attempt to call it.  */
}
