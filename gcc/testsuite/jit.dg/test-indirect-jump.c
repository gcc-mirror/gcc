#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     int
     test_indirect_jump (int idx)
     {
       void *targets[3];
       targets[0] = &&b_zero;
       targets[1] = &&b_one;
       targets[2] = &&b_two;
       goto *targets[idx];

     b_zero:
       return 10;
     b_one:
       return 20;
     b_two:
       return 30;
     }
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *void_ptr_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID_PTR);
  gcc_jit_type *array_type =
    gcc_jit_context_new_array_type (ctxt, NULL, void_ptr_type, 3);

  gcc_jit_param *param_idx =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "idx");
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "test_indirect_jump",
				  1, &param_idx,
				  0);

  gcc_jit_block *b_entry = gcc_jit_function_new_block (func, "entry");
  gcc_jit_block *b_zero = gcc_jit_function_new_block (func, "b_zero");
  gcc_jit_block *b_one = gcc_jit_function_new_block (func, "b_one");
  gcc_jit_block *b_two = gcc_jit_function_new_block (func, "b_two");

  gcc_jit_lvalue *targets =
    gcc_jit_function_new_local (func, NULL, array_type, "targets");

  gcc_jit_block *blocks[3] = { b_zero, b_one, b_two };
  for (int i = 0; i < 3; i++)
    {
      gcc_jit_rvalue *idx_val =
	gcc_jit_context_new_rvalue_from_int (ctxt, int_type, i);
      gcc_jit_lvalue *elem =
	gcc_jit_context_new_array_access (ctxt, NULL,
					  gcc_jit_lvalue_as_rvalue (targets),
					  idx_val);
      gcc_jit_rvalue *blk_addr =
	gcc_jit_block_get_address (blocks[i], NULL);
      gcc_jit_block_add_assignment (b_entry, NULL, elem, blk_addr);
    }

  /* void *target_addr = targets[idx]; */
  gcc_jit_lvalue *selected_elem =
    gcc_jit_context_new_array_access (ctxt, NULL,
				      gcc_jit_lvalue_as_rvalue (targets),
				      gcc_jit_param_as_rvalue (param_idx));

  /* goto *targets[idx]; */
  gcc_jit_block_end_with_indirect_jump (
    b_entry, NULL,
    gcc_jit_lvalue_as_rvalue (selected_elem),
    3, blocks);

  /* b_zero: return 10; */
  gcc_jit_block_end_with_return (
    b_zero, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 10));

  /* b_one: return 20; */
  gcc_jit_block_end_with_return (
    b_one, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 20));

  /* b_two: return 30; */
  gcc_jit_block_end_with_return (
    b_two, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 30));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)

{
  typedef int (*fn_type) (int);
  CHECK_NON_NULL (result);

  fn_type test_indirect_jump =
    (fn_type)gcc_jit_result_get_code (result, "test_indirect_jump");
  CHECK_NON_NULL (test_indirect_jump);

  CHECK_VALUE (test_indirect_jump (0), 10);
  CHECK_VALUE (test_indirect_jump (1), 20);
  CHECK_VALUE (test_indirect_jump (2), 30);
}
