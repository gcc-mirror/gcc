#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

#ifdef __cplusplus
extern "C" {
#endif

  extern int imported_global;

#ifdef __cplusplus
}
#endif

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     int exported_global;
     extern int imported_global;
     static int internal_global;

     int
     test_using_global (void)
     {
	exported_global += 1;
	imported_global += 1;
	internal_global += 1;
	return internal_global;
     }
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_lvalue *exported_global =
    gcc_jit_context_new_global (ctxt,
				NULL,
				GCC_JIT_GLOBAL_EXPORTED,
				int_type,
				"exported_global");
  gcc_jit_lvalue *imported_global =
    gcc_jit_context_new_global (ctxt,
				NULL,
				GCC_JIT_GLOBAL_IMPORTED,
				int_type,
				"imported_global");
  gcc_jit_lvalue *internal_global =
    gcc_jit_context_new_global (ctxt,
				NULL,
				GCC_JIT_GLOBAL_INTERNAL,
				int_type,
				"internal_global");

  /* Build the test_fn.  */
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "test_using_global",
				  0, NULL,
				  0);
  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);

  gcc_jit_block_add_assignment_op (
    block, NULL,
    exported_global,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_one (ctxt, int_type));
  gcc_jit_block_add_assignment_op (
    block, NULL,
    imported_global,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_one (ctxt, int_type));
  gcc_jit_block_add_assignment_op (
    block, NULL,
    internal_global,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_context_one (ctxt, int_type));
  gcc_jit_block_end_with_return (block,
				 NULL,
				 gcc_jit_lvalue_as_rvalue (internal_global));
}

int imported_global;

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*fn_type) (void);
  CHECK_NON_NULL (result);

  fn_type test_using_global =
    (fn_type)gcc_jit_result_get_code (result, "test_using_global");
  CHECK_NON_NULL (test_using_global);

  /* The exported global should be visible.  */
  int *exported_global = (int *)gcc_jit_result_get_global (result, "exported_global");
  CHECK_NON_NULL (exported_global);
  /* ...and should be zero-initialized.  */
  CHECK_VALUE (*exported_global, 0);

  /* Set some nonzero values.  */
  *exported_global = 11;
  imported_global = 42;

  /* The internal global shouldn't be visible.  */
  int *internal_global = (int *)gcc_jit_result_get_global (result, "internal_global");
  CHECK_VALUE (internal_global, NULL);

  /* Call the JIT-generated function.  */
  int call_count = test_using_global ();

  /* Verify that it correctly modified imported_global and exported_global.  */
  CHECK_VALUE (*exported_global, 12);
  CHECK_VALUE (imported_global, 43);
  CHECK_VALUE (call_count, 1);

  /* Try calling it again.  */
  call_count = test_using_global ();

  /* Verify the new values.  */
  CHECK_VALUE (*exported_global, 13);
  CHECK_VALUE (imported_global, 44);
  CHECK_VALUE (call_count, 2);
}

