#include <libgccjit.h>
#include "harness.h"

struct my_struct { long a; long b; long c; };

void create_code (gcc_jit_context *ctxt, void *user_data)
{
	/* Create the equivalent of:
		struct my_struct { long a; long b; long c; };
		static struct my_struct deref(struct my_struct *ptr) { return *ptr; } 
		long get_a(struct my_struct *s) { return deref(s).a; }
	   and compile it at -O1.  */
	gcc_jit_context_set_int_option(ctxt, GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL, 1);

	gcc_jit_type *long_type = gcc_jit_context_get_type(ctxt, GCC_JIT_TYPE_LONG);
	gcc_jit_field* fields[3] = {
		gcc_jit_context_new_field(ctxt, NULL, long_type, "a"),
		gcc_jit_context_new_field(ctxt, NULL, long_type, "b"),
		gcc_jit_context_new_field(ctxt, NULL, long_type, "c"),
	};
	gcc_jit_struct *my_struct =
		gcc_jit_context_new_struct_type(ctxt, NULL, "my_struct", 3, fields);
	gcc_jit_type *my_struct_type = gcc_jit_struct_as_type(my_struct);
	gcc_jit_type *my_struct_ptr_type = gcc_jit_type_get_pointer(my_struct_type);

	/* struct my_struct deref(struct my_struct *ptr) { return *ptr; } */
	gcc_jit_param *param_deref =
		gcc_jit_context_new_param(ctxt, NULL, my_struct_ptr_type, "ptr");
	gcc_jit_function *func_deref = gcc_jit_context_new_function(
		ctxt, NULL, GCC_JIT_FUNCTION_INTERNAL,
		my_struct_type, "deref",
		1, &param_deref,
		0);
	gcc_jit_block *blockDeref = gcc_jit_function_new_block(func_deref, NULL);
	gcc_jit_block_end_with_return(
		blockDeref, NULL,
		gcc_jit_lvalue_as_rvalue(gcc_jit_rvalue_dereference(gcc_jit_param_as_rvalue(param_deref), NULL)));

	/* long get_a(struct my_struct *s) { return deref(s).a; } */
	gcc_jit_param *param_get_a = gcc_jit_context_new_param(ctxt, NULL, my_struct_ptr_type, "s");
	gcc_jit_function *func_get_a = gcc_jit_context_new_function(
		ctxt, NULL, GCC_JIT_FUNCTION_EXPORTED,
		long_type, "get_a",
		1, &param_get_a,
		0);
	gcc_jit_block *block_get_a = gcc_jit_function_new_block(func_get_a, NULL);
	gcc_jit_rvalue *argsForDeref[1] = {gcc_jit_param_as_rvalue(param_get_a)};
	gcc_jit_rvalue *callDeref = gcc_jit_context_new_call(ctxt, NULL, func_deref, 1, argsForDeref);
	gcc_jit_block_end_with_return(
		block_get_a, NULL,
		gcc_jit_rvalue_access_field(callDeref, NULL, fields[0]));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef long(*fn_type) (struct my_struct*);
  fn_type get_a = (fn_type) gcc_jit_result_get_code(result, "get_a");

  struct my_struct s = {1, 2, 3};
  CHECK_VALUE (get_a(&s), 1);
}
