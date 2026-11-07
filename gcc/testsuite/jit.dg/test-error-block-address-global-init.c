#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *void_ptr_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID_PTR);

  gcc_jit_function *fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  void_type,
				  "my_fn",
				  0, NULL,
				  0);
  gcc_jit_block *block =
    gcc_jit_function_new_block (fn, "target_block");

  gcc_jit_rvalue *block_addr =
    gcc_jit_block_get_address (block, NULL);

  /* 1. Direct global initializer (should fail validation).  */
  gcc_jit_lvalue *global_ptr =
    gcc_jit_context_new_global (ctxt, NULL,
				GCC_JIT_GLOBAL_EXPORTED,
				void_ptr_type,
				"global_ptr");
  CHECK_VALUE (
    gcc_jit_global_set_initializer_rvalue (global_ptr, block_addr),
    NULL);

  /* 2. Nested inside an array constructor (should fail validation).  */
  gcc_jit_type *array_type =
    gcc_jit_context_new_array_type (ctxt, NULL, void_ptr_type, 1);
  gcc_jit_lvalue *global_table =
    gcc_jit_context_new_global (ctxt, NULL,
				GCC_JIT_GLOBAL_EXPORTED,
				array_type,
				"global_table");
  gcc_jit_rvalue *ctor =
    gcc_jit_context_new_array_constructor (ctxt, NULL,
					   array_type,
					   1, &block_addr);
  CHECK_VALUE (
    gcc_jit_global_set_initializer_rvalue (global_table, ctor),
    NULL);

  /* Consume ctor (and transitively block_addr) within my_fn so the
     generated reproducer reads the variables and avoids
     -Werror=unused-but-set-variable.  */
  gcc_jit_block_add_eval (block, NULL, ctor);
  gcc_jit_block_end_with_void_return (block, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  CHECK_STRING_VALUE (
    gcc_jit_context_get_first_error (ctxt),
    "gcc_jit_global_set_initializer_rvalue:"
    " rvalue &&target_block (type: void *)"
    " has scope limited to function my_fn"
    " but was used in initializer for global global_ptr");

  CHECK_STRING_VALUE (
    gcc_jit_context_get_last_error (ctxt),
    "gcc_jit_global_set_initializer_rvalue:"
    " rvalue &&target_block (type: void *)"
    " has scope limited to function my_fn"
    " but was used in initializer for global global_table");
}
