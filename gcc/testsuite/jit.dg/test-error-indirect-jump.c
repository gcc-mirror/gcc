#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to trigger every validation error on
     gcc_jit_block_get_address and gcc_jit_block_end_with_indirect_jump.  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_function *fn1 =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  void_type,
				  "fn1",
				  0, NULL,
				  0);
  gcc_jit_block *b_fn1 =
    gcc_jit_function_new_block (fn1, "b_fn1");

  gcc_jit_function *fn2 =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  void_type,
				  "fn2",
				  0, NULL,
				  0);
  gcc_jit_block *b_fn2 =
    gcc_jit_function_new_block (fn2, "b_fn2");

  /* 1. Non-pointer target_address.  */
  gcc_jit_rvalue * non_ptr =
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 42);
  gcc_jit_block_end_with_indirect_jump (b_fn1, NULL, non_ptr, 0, NULL);
  CHECK_STRING_VALUE (
    gcc_jit_context_get_last_error (ctxt),
    "gcc_jit_block_end_with_indirect_jump:"
    " target_address: (int)42 is not of a pointer type");

  /* 2. Negative num_destinations.  */
  gcc_jit_rvalue *addr_fn1 = gcc_jit_block_get_address (b_fn1, NULL);
  gcc_jit_block_end_with_indirect_jump (b_fn1, NULL, addr_fn1, -1, NULL);
  CHECK_STRING_VALUE (
    gcc_jit_context_get_last_error (ctxt),
    "gcc_jit_block_end_with_indirect_jump: num_destinations < 0");

  /* 3. NULL destinations array when num_destinations > 0.  */
  gcc_jit_block_end_with_indirect_jump (b_fn1, NULL, addr_fn1, 1, NULL);
  CHECK_STRING_VALUE (
    gcc_jit_context_get_last_error (ctxt),
    "gcc_jit_block_end_with_indirect_jump: NULL destinations");

  /* 4. NULL element in destinations array.  */
  gcc_jit_block *null_dests[1] = { NULL };
  gcc_jit_block_end_with_indirect_jump (b_fn1, NULL, addr_fn1, 1, null_dests);
  CHECK_STRING_VALUE (
    gcc_jit_context_get_last_error (ctxt),
    "gcc_jit_block_end_with_indirect_jump:"
    " NULL destination block at index 0");

  /* 5. Destination block in wrong function.  */
  gcc_jit_block *wrong_fn_dests[1] = { b_fn2 };
  gcc_jit_block_end_with_indirect_jump (b_fn1, NULL, addr_fn1, 1,
					wrong_fn_dests);
  CHECK_STRING_VALUE (
    gcc_jit_context_get_last_error (ctxt),
    "gcc_jit_block_end_with_indirect_jump:"
    " destination block 0 (b_fn2) is not in the same function"
    " (function fn2) as source block b_fn1");

  /* Terminate b_fn1 cleanly so function validation doesn't overwrite
     last_error with an "unterminated block" error during compile.  */
  gcc_jit_block_end_with_void_return (b_fn1, NULL);

  /* 6. Cross-function block address (addr_fn1 used inside fn2).  */
  gcc_jit_block *fn2_dests[1] = { b_fn2 };
  gcc_jit_block_end_with_indirect_jump (b_fn2, NULL, addr_fn1, 1,
					fn2_dests);

  CHECK_STRING_VALUE (
    gcc_jit_context_get_last_error (ctxt),
    "gcc_jit_block_end_with_indirect_jump:"
    " rvalue &&b_fn1 (type: void *) has scope limited to function fn1"
    " but was used within function fn2"
    " (in statement: goto *&&b_fn1;)");
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  CHECK_STRING_VALUE (
    gcc_jit_context_get_first_error (ctxt),
    "gcc_jit_block_end_with_indirect_jump:"
    " target_address: (int)42 is not of a pointer type");

  /* Verify that using a block address from fn1 inside fn2 was caught
     by verify_valid_within_stmt.  */
  CHECK_STRING_VALUE (
    gcc_jit_context_get_last_error (ctxt),
    "gcc_jit_block_end_with_indirect_jump:"
    " rvalue &&b_fn1 (type: void *) has scope limited to function fn1"
    " but was used within function fn2"
    " (in statement: goto *&&b_fn1;)");
}
