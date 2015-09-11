#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

/* Reproducer for PR jit/66779.

   Inject the equivalent of:
     T FUNCNAME (T i, T j, T k)
     {
       bool comp0 = i & 0x40;
       bool comp1 = (j == k);
       if (comp0 && comp1)
	 return 7;
       else
	 return 22;
     }
   for some type T; this was segfaulting during the expansion to RTL
   due to missing handling for some machine modes in
   jit_langhook_type_for_mode.  */

void
create_fn (gcc_jit_context *ctxt,
	   const char *funcname,
	   enum gcc_jit_types jit_type)
{
  gcc_jit_type *the_type =
    gcc_jit_context_get_type (ctxt, jit_type);
  gcc_jit_type *t_bool =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_BOOL);
  gcc_jit_param *param_i =
    gcc_jit_context_new_param (ctxt, NULL, the_type, "i");
  gcc_jit_param *param_j =
    gcc_jit_context_new_param (ctxt, NULL, the_type, "j");
  gcc_jit_param *param_k =
    gcc_jit_context_new_param (ctxt, NULL, the_type, "k");
  gcc_jit_param *params[3] = {
    param_i,
    param_j,
    param_k
  };
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  the_type,
				  funcname,
				  3, params,
				  0);
  gcc_jit_block *b_entry = gcc_jit_function_new_block (func, "entry");
  gcc_jit_block *b_on_true = gcc_jit_function_new_block (func, "on_true");
  gcc_jit_block *b_on_false = gcc_jit_function_new_block (func, "on_false");

  gcc_jit_lvalue *comp0 =
    gcc_jit_function_new_local (func, NULL, t_bool, "comp0");

  gcc_jit_block_add_assignment (
    b_entry, NULL,
    comp0,
    gcc_jit_context_new_comparison (
      ctxt, NULL,
      GCC_JIT_COMPARISON_NE,
      gcc_jit_context_new_binary_op (
	ctxt, NULL,
	GCC_JIT_BINARY_OP_BITWISE_AND,
	the_type,
	gcc_jit_param_as_rvalue (param_i),
	gcc_jit_context_new_rvalue_from_int (ctxt, the_type, 0x40)),
      gcc_jit_context_zero (ctxt, the_type)));

  gcc_jit_lvalue *comp1 =
    gcc_jit_function_new_local (func, NULL, t_bool, "comp1");

  gcc_jit_block_add_assignment (
    b_entry, NULL,
    comp1,
    gcc_jit_context_new_comparison (ctxt, NULL,
				    GCC_JIT_COMPARISON_EQ,
				    gcc_jit_param_as_rvalue (param_j),
				    gcc_jit_param_as_rvalue (param_k)));

 gcc_jit_rvalue *cond =
   gcc_jit_context_new_binary_op (ctxt, NULL,
				  GCC_JIT_BINARY_OP_LOGICAL_AND,
				  t_bool,
				  gcc_jit_lvalue_as_rvalue (comp0),
				  gcc_jit_lvalue_as_rvalue (comp1));

  gcc_jit_block_end_with_conditional (b_entry, NULL,
				      cond,
				      b_on_true,
				      b_on_false);

  gcc_jit_block_end_with_return (
    b_on_true, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, the_type, 7));

  gcc_jit_block_end_with_return (
    b_on_false, NULL,
    gcc_jit_context_new_rvalue_from_int (ctxt, the_type, 22));
}

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  create_fn (ctxt, "pr66779_signed_char", GCC_JIT_TYPE_SIGNED_CHAR);
  create_fn (ctxt, "pr66779_unsigned_char", GCC_JIT_TYPE_UNSIGNED_CHAR);

  create_fn (ctxt, "pr66779_short", GCC_JIT_TYPE_SHORT);
  create_fn (ctxt, "pr66779_unsigned_short", GCC_JIT_TYPE_UNSIGNED_SHORT);

  create_fn (ctxt, "pr66779_int", GCC_JIT_TYPE_INT);
  create_fn (ctxt, "pr66779_unsigned_int", GCC_JIT_TYPE_UNSIGNED_INT);

  create_fn (ctxt, "pr66779_long", GCC_JIT_TYPE_LONG);
  create_fn (ctxt, "pr66779_unsigned_long", GCC_JIT_TYPE_UNSIGNED_LONG);

  create_fn (ctxt, "pr66779_long_long",
	     GCC_JIT_TYPE_LONG_LONG);
  create_fn (ctxt, "pr66779_unsigned_long_long",
	     GCC_JIT_TYPE_UNSIGNED_LONG_LONG);

  create_fn (ctxt, "pr66779_size_t", GCC_JIT_TYPE_SIZE_T);
}

extern void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*fn_type) (int, int, int);
  CHECK_NON_NULL (result);
  /* Sanity-check the "int" case.  */
  fn_type fn =
    (fn_type)gcc_jit_result_get_code (result, "pr66779_int");
  CHECK_NON_NULL (fn);
  CHECK_VALUE (fn (0, 0, 0), 22);
  CHECK_VALUE (fn (0, 0, 1), 22);
  CHECK_VALUE (fn (0x40, 0, 0), 7);
  CHECK_VALUE (fn (0x40, 0, 1), 22);
  CHECK_VALUE (fn (0x40, 1, 1), 7);
  CHECK_VALUE (fn (0x3f, 0, 0), 22);
  CHECK_VALUE (fn (0x3f, 1, 1), 22);
}
