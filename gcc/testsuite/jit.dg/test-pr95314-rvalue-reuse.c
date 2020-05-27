#include <libgccjit.h>
#include "harness.h"

void create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *t_int =  gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *t_void = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *t_const_char_ptr
    = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CONST_CHAR_PTR);
  gcc_jit_lvalue *global
    = gcc_jit_context_new_global (ctxt, NULL, GCC_JIT_GLOBAL_INTERNAL,
				  t_const_char_ptr, "pr95314_global");

  gcc_jit_rvalue *global_ref = gcc_jit_lvalue_get_address(global, NULL);

  gcc_jit_param *param_string
    = gcc_jit_context_new_param (ctxt, NULL, t_const_char_ptr, "string");
  gcc_jit_function *puts_func
    = gcc_jit_context_new_function (ctxt, NULL, GCC_JIT_FUNCTION_IMPORTED,
				    t_int, "puts", 1, &param_string, 0);

#define NUM_INNER_FNS 3
  gcc_jit_function *inner_fns[NUM_INNER_FNS];
  for (int i = 0; i < NUM_INNER_FNS; i++)
    {
      char fnname[128];
      sprintf (fnname, "pr95314_inner_%i", i);
      inner_fns[i]
	= gcc_jit_context_new_function (ctxt, NULL, GCC_JIT_FUNCTION_INTERNAL,
					t_void, fnname, 0, NULL, 0);
      gcc_jit_block *block = gcc_jit_function_new_block (inner_fns[i], NULL);
      gcc_jit_rvalue *arg
	= gcc_jit_context_new_cast (ctxt, NULL, global_ref, t_const_char_ptr);
      gcc_jit_block_add_eval (block, NULL,
			      gcc_jit_context_new_call (ctxt, NULL, puts_func,
							1, &arg));
      gcc_jit_block_end_with_void_return (block, NULL);
    }

  gcc_jit_function *outer_func
    = gcc_jit_context_new_function (ctxt, NULL, GCC_JIT_FUNCTION_EXPORTED,
				    t_void, "pr95314_outer", 0, NULL, 0);
  gcc_jit_block *block = gcc_jit_function_new_block (outer_func, NULL);
  for (int i = 0; i < NUM_INNER_FNS; i++)
    gcc_jit_block_add_eval (block, NULL,
			    gcc_jit_context_new_call (ctxt, NULL, inner_fns[i],
						      0, NULL));
  gcc_jit_block_end_with_void_return (block, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);
  (void)gcc_jit_result_get_code (result, "pr95314_outer");
}
