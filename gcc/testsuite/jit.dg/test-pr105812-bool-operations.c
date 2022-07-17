#include "libgccjit.h"

#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type* bool_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_BOOL);
  gcc_jit_type* bool_ptr_type =
    gcc_jit_type_get_pointer (gcc_jit_type_get_aligned (bool_type, 1));

  /* Function 1 */

  gcc_jit_param* param1 = gcc_jit_context_new_param (ctxt, NULL, bool_type,
						     "param1");
  gcc_jit_function* function1 =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED, bool_type,
				  "function1", 1, &param1, 0);
  gcc_jit_block* block1 = gcc_jit_function_new_block (function1, "start1");

  gcc_jit_lvalue* var1 =
    gcc_jit_function_new_local (function1, NULL, bool_type, "var1");
  gcc_jit_rvalue* addr1 =
    gcc_jit_lvalue_get_address (var1, NULL);
  gcc_jit_rvalue* ptr1 =
    gcc_jit_context_new_cast (ctxt, NULL, addr1, bool_ptr_type);
  gcc_jit_lvalue* deref1 =
    gcc_jit_rvalue_dereference (ptr1, NULL);
  gcc_jit_rvalue* param1_rvalue =
    gcc_jit_param_as_rvalue (param1);
  gcc_jit_block_add_assignment (block1, NULL, deref1, param1_rvalue);

  gcc_jit_rvalue* one = gcc_jit_context_one (ctxt, bool_type);
  gcc_jit_block_end_with_return (block1, NULL, one);

  /* Function 2 */

  gcc_jit_param* param2 = gcc_jit_context_new_param (ctxt, NULL, bool_type,
						     "param2");
  gcc_jit_function* function2 =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED, bool_type,
				  "function2", 1, &param2, 0);
  gcc_jit_block* block2 = gcc_jit_function_new_block (function2, "start2");

  gcc_jit_lvalue* var2 =
    gcc_jit_function_new_local (function2, NULL, bool_type, "var2");
  gcc_jit_rvalue* addr2 =
    gcc_jit_lvalue_get_address (var2, NULL);
  gcc_jit_rvalue* ptr2 =
    gcc_jit_context_new_cast (ctxt, NULL, addr2, bool_ptr_type);
  gcc_jit_lvalue* deref2 =
    gcc_jit_rvalue_dereference (ptr2, NULL);
  gcc_jit_rvalue* param2_rvalue =
    gcc_jit_param_as_rvalue (param2);
  gcc_jit_block_add_assignment (block2, NULL, deref2, param2_rvalue);

  gcc_jit_lvalue* return_value =
    gcc_jit_function_new_local (function2, NULL, bool_type, "return_value");
  gcc_jit_rvalue* call =
    gcc_jit_context_new_call (ctxt, NULL, function1, 1, &param2_rvalue);
  gcc_jit_block_add_assignment (block2, NULL, return_value, call);

  gcc_jit_block* block2_1 =
    gcc_jit_function_new_block (function2, "end2");
  gcc_jit_block_end_with_jump (block2, NULL, block2_1);

  gcc_jit_rvalue* value =
    gcc_jit_context_new_unary_op (ctxt, NULL,
				  GCC_JIT_UNARY_OP_LOGICAL_NEGATE, bool_type,
				  param2_rvalue);
  gcc_jit_rvalue* return_rvalue =
    gcc_jit_lvalue_as_rvalue (return_value);
  gcc_jit_rvalue* and =
    gcc_jit_context_new_binary_op (ctxt, NULL,
				   GCC_JIT_BINARY_OP_BITWISE_AND, bool_type,
				   return_rvalue, value);

  gcc_jit_block_end_with_return (block2_1, NULL, and);
}

extern void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* Verify that no errors were emitted.  */
  CHECK_NON_NULL (result);
}
