#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <stdbool.h>

#include "libgccjit.h"

#include "harness.h"

static void
create_overflow_fn (gcc_jit_context *ctxt,
		    gcc_jit_type *type,
		    const char *funcname,
		    const char *builtin_name)
{
  /* Create the equivalent of this C:

       int
       test_overflow_T_OP (T x, T y, bool *ovf)
       {
	 T result;
	 result = x OP y;
	 *ovf = ...; // did overflow happen?
	 return result;
       }

  */
  gcc_jit_type *t_bool =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_BOOL);
  gcc_jit_type *t_bool_star =
    gcc_jit_type_get_pointer (t_bool);

  gcc_jit_param *x =
    gcc_jit_context_new_param (
      ctxt,
      NULL,
      type, "x");
  gcc_jit_param *y =
    gcc_jit_context_new_param (
      ctxt,
      NULL,
      type, "y");
  gcc_jit_param *ovf =
    gcc_jit_context_new_param (
      ctxt,
      NULL,
      t_bool_star, "ovf");
  gcc_jit_param *params[3] = {x, y, ovf};

  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt,
				  NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  type,
				  funcname,
				  3, params, 0);

  gcc_jit_lvalue *result =
    gcc_jit_function_new_local (func, NULL, type, "result");

  gcc_jit_block *b_initial =
    gcc_jit_function_new_block (func, "initial");

  /* The builtins are listed in builtins.def as being variadic, but
     the have these signatures:
       bool __builtin_add_overflow (type1 a, type2 b, type3 *res);
       bool __builtin_sub_overflow (type1 a, type2 b, type3 *res);
       bool __builtin_mul_overflow (type1 a, type2 b, type3 *res);  */

  gcc_jit_function *builtin_fn =
    gcc_jit_context_get_builtin_function (ctxt, builtin_name);

  /* Construct a call of the form:
       (returns bool) __builtin_add_overflow (x, y, &result).  */
  gcc_jit_rvalue *args[3] = {gcc_jit_param_as_rvalue (x),
			     gcc_jit_param_as_rvalue (y),
			     gcc_jit_lvalue_get_address (result, NULL)};
  gcc_jit_rvalue *call =
    gcc_jit_context_new_call (ctxt,
			      NULL,
			      builtin_fn,
			      3, args);

  /* "*ovf = BUILTIN_CALL ();" */
  gcc_jit_block_add_assignment (
    b_initial, NULL,
    gcc_jit_rvalue_dereference (gcc_jit_param_as_rvalue (ovf),
				NULL),
    call);

  /* "return result;" */
  gcc_jit_block_end_with_return (
    b_initial, NULL,
    gcc_jit_lvalue_as_rvalue (result));
}

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* int */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  create_overflow_fn (ctxt, int_type,
		      "test_overflow_int_add",
		      "__builtin_add_overflow");
  create_overflow_fn (ctxt, int_type,
		      "test_overflow_int_sub",
		      "__builtin_sub_overflow");
  create_overflow_fn (ctxt, int_type,
		      "test_overflow_int_mul",
		      "__builtin_mul_overflow");

  /* uint */
  gcc_jit_type *uint_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UNSIGNED_INT);
  create_overflow_fn (ctxt, uint_type,
		      "test_overflow_uint_add",
		      "__builtin_add_overflow");
  create_overflow_fn (ctxt, uint_type,
		      "test_overflow_uint_sub",
		      "__builtin_sub_overflow");
  create_overflow_fn (ctxt, uint_type,
		      "test_overflow_uint_mul",
		      "__builtin_mul_overflow");
}

void
verify_int_overflow_fn (gcc_jit_result *jit_result,
			const char *funcname,
			int x, int y,
			int expected_result,
			bool expected_ovf)
{
  CHECK_NON_NULL (jit_result);
  typedef int (*overflow_fn_type) (int, int, bool *);
  overflow_fn_type fn =
    (overflow_fn_type)gcc_jit_result_get_code (jit_result, funcname);
  CHECK_NON_NULL (fn);

  /* Call the function:  */
  bool actual_ovf = 0;
  int actual_result = fn (x, y, &actual_ovf);
  note ("%s (%d, %d) returned: %d with ovf: %d",
	funcname, x, y, actual_result, actual_ovf);
  CHECK_VALUE (actual_result, expected_result);
  CHECK_VALUE (actual_ovf, expected_ovf);
}

void
verify_uint_overflow_fn (gcc_jit_result *jit_result,
			 const char *funcname,
			 unsigned int x, unsigned int y,
			 unsigned int expected_result,
			 bool expected_ovf)
{
  CHECK_NON_NULL (jit_result);
  typedef unsigned int (*overflow_fn_type) (unsigned int, unsigned int,
					    bool *);
  overflow_fn_type fn =
    (overflow_fn_type)gcc_jit_result_get_code (jit_result, funcname);
  CHECK_NON_NULL (fn);

  /* Call the function:  */
  bool actual_ovf = 0;
  unsigned int actual_result = fn (x, y, &actual_ovf);
  note ("%s (%d, %d) returned: %d with ovf: %d",
	funcname, x, y, actual_result, actual_ovf);
  CHECK_VALUE (actual_result, expected_result);
  CHECK_VALUE (actual_ovf, expected_ovf);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  verify_int_overflow_fn (result, "test_overflow_int_add",
			  5, 15,
			  20, 0);
  verify_int_overflow_fn (result, "test_overflow_int_add",
			  INT_MAX, 1,
			  INT_MIN, 1);
  verify_int_overflow_fn (result, "test_overflow_int_sub",
			  5, 15,
			  -10, 0);
  verify_int_overflow_fn (result, "test_overflow_int_sub",
			  INT_MIN, 1,
			  INT_MAX, 1);
  verify_int_overflow_fn (result, "test_overflow_int_mul",
			  5, 15,
			  75, 0);
  verify_int_overflow_fn (result, "test_overflow_int_mul",
			  INT_MAX, 1,
			  INT_MAX, 0);
  verify_int_overflow_fn (result, "test_overflow_int_mul",
			  INT_MAX, 2,
			  -2, 1);

  verify_uint_overflow_fn (result, "test_overflow_uint_add",
			   5, 15,
			   20, 0);
  verify_uint_overflow_fn (result, "test_overflow_uint_add",
			   INT_MAX, 1,
			   (((unsigned int)INT_MAX) + 1), 0);
  verify_uint_overflow_fn (result, "test_overflow_uint_add",
			   UINT_MAX, 1,
			   0, 1);
  verify_uint_overflow_fn (result, "test_overflow_uint_sub",
			   5, 15,
			   (UINT_MAX - 9), 1);
  verify_uint_overflow_fn (result, "test_overflow_uint_sub",
			   INT_MIN, 1,
			   ((unsigned int)INT_MIN - 1), 0);
  verify_uint_overflow_fn (result, "test_overflow_uint_mul",
			   5, 15,
			   75, 0);
  verify_uint_overflow_fn (result, "test_overflow_uint_mul",
			   INT_MAX, 1,
			   INT_MAX, 0);
  verify_uint_overflow_fn (result, "test_overflow_uint_mul",
			   INT_MAX, 2,
			   (unsigned int)INT_MAX * 2, 0);
  verify_uint_overflow_fn (result, "test_overflow_uint_mul",
			   UINT_MAX, 2,
			   -2/*(unsigned int)INT_MAX * 2*/, 1);
}
