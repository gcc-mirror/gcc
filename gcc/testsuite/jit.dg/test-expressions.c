#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include <stdbool.h>

#include "libgccjit.h"

#include "harness.h"

/**********************************************************************
 Unary ops
 **********************************************************************/

static const char *
make_test_of_unary_op (gcc_jit_context *ctxt,
		       gcc_jit_type *type,
		       enum gcc_jit_unary_op op,
		       const char *funcname)
{
  /* Make a test function of the form:
       T test_unary_op (T a)
       {
	  return OP a;
       }
     and return a debug dump of the OP so that
     the caller can sanity-check the debug dump implementation.
  */
  gcc_jit_param *param_a =
    gcc_jit_context_new_param (ctxt, NULL, type, "a");
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  type,
				  funcname,
				  1, &param_a,
				  0);
  gcc_jit_rvalue *unary_op = gcc_jit_context_new_unary_op (
    ctxt,
    NULL,
    op,
    type,
    gcc_jit_param_as_rvalue (param_a));

  gcc_jit_block *initial = gcc_jit_function_new_block (test_fn, "initial");
  gcc_jit_block_end_with_return (initial, NULL, unary_op);

  return gcc_jit_object_get_debug_string (
    gcc_jit_rvalue_as_object (unary_op));
}


static void
make_tests_of_unary_ops (gcc_jit_context *ctxt)
{
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  CHECK_STRING_VALUE (
    make_test_of_unary_op (ctxt,
			   int_type,
			   GCC_JIT_UNARY_OP_MINUS,
			   "test_UNARY_OP_MINUS_on_int"),
    "-(a)");
  CHECK_STRING_VALUE (
    make_test_of_unary_op (ctxt,
			   int_type,
			   GCC_JIT_UNARY_OP_BITWISE_NEGATE,
			   "test_UNARY_OP_BITWISE_NEGATE_on_int"),
    "~(a)");
  CHECK_STRING_VALUE (
    make_test_of_unary_op (ctxt,
			   int_type,
			   GCC_JIT_UNARY_OP_LOGICAL_NEGATE,
			   "test_UNARY_OP_LOGICAL_NEGATE_on_int"),
    "!(a)");
  CHECK_STRING_VALUE (
    make_test_of_unary_op (ctxt,
			   int_type,
			   GCC_JIT_UNARY_OP_ABS,
			   "test_UNARY_OP_ABS_on_int"),
    "abs (a)");
}

static void
verify_unary_ops (gcc_jit_result *result)
{
  typedef int (*test_fn) (int);

  test_fn test_UNARY_OP_MINUS_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_UNARY_OP_MINUS_on_int");
  CHECK_NON_NULL (test_UNARY_OP_MINUS_on_int);
  CHECK_VALUE (test_UNARY_OP_MINUS_on_int (0), 0);
  CHECK_VALUE (test_UNARY_OP_MINUS_on_int (42), -42);
  CHECK_VALUE (test_UNARY_OP_MINUS_on_int (-5), 5);

  test_fn test_UNARY_OP_BITWISE_NEGATE_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_UNARY_OP_BITWISE_NEGATE_on_int");
  CHECK_NON_NULL (test_UNARY_OP_BITWISE_NEGATE_on_int);
  CHECK_VALUE (test_UNARY_OP_BITWISE_NEGATE_on_int (0), ~0);
  CHECK_VALUE (test_UNARY_OP_BITWISE_NEGATE_on_int (42), ~42);
  CHECK_VALUE (test_UNARY_OP_BITWISE_NEGATE_on_int (-5), ~-5);

  test_fn test_UNARY_OP_LOGICAL_NEGATE_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_UNARY_OP_LOGICAL_NEGATE_on_int");
  CHECK_NON_NULL (test_UNARY_OP_LOGICAL_NEGATE_on_int);
  CHECK_VALUE (test_UNARY_OP_LOGICAL_NEGATE_on_int (0), 1);
  CHECK_VALUE (test_UNARY_OP_LOGICAL_NEGATE_on_int (42), 0);
  CHECK_VALUE (test_UNARY_OP_LOGICAL_NEGATE_on_int (-5), 0);

  test_fn test_UNARY_OP_ABS_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_UNARY_OP_ABS_on_int");
  CHECK_NON_NULL (test_UNARY_OP_ABS_on_int);
  CHECK_VALUE (test_UNARY_OP_ABS_on_int (0), 0);
  CHECK_VALUE (test_UNARY_OP_ABS_on_int (42), 42);
  CHECK_VALUE (test_UNARY_OP_ABS_on_int (-5), 5);
}

/**********************************************************************
 Binary ops
 **********************************************************************/

static const char *
make_test_of_binary_op (gcc_jit_context *ctxt,
			gcc_jit_type *type,
			enum gcc_jit_binary_op op,
			const char *funcname)
{
  /* Make a test function of the form:
       T test_binary_op (T a, T b)
       {
	  return a OP b;
       }
  */
  gcc_jit_param *param_a =
    gcc_jit_context_new_param (ctxt, NULL, type, "a");
  gcc_jit_param *param_b =
    gcc_jit_context_new_param (ctxt, NULL, type, "b");
  gcc_jit_param *params[] = {param_a, param_b};
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  type,
				  funcname,
				  2, params,
				  0);
  gcc_jit_rvalue *binary_op =
    gcc_jit_context_new_binary_op (
      ctxt,
      NULL,
      op,
      type,
      gcc_jit_param_as_rvalue (param_a),
      gcc_jit_param_as_rvalue (param_b));

  gcc_jit_block *initial = gcc_jit_function_new_block (test_fn, "initial");
  gcc_jit_block_end_with_return (initial, NULL, binary_op);

  return gcc_jit_object_get_debug_string (
    gcc_jit_rvalue_as_object (binary_op));
}


static void
make_tests_of_binary_ops (gcc_jit_context *ctxt)
{
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  /* Test binary ops.  */
  CHECK_STRING_VALUE (
    make_test_of_binary_op (ctxt,
			    int_type,
			    GCC_JIT_BINARY_OP_PLUS,
			    "test_BINARY_OP_PLUS_on_int"),
    "a + b");
  CHECK_STRING_VALUE (
    make_test_of_binary_op (ctxt,
			    int_type,
			    GCC_JIT_BINARY_OP_MINUS,
			    "test_BINARY_OP_MINUS_on_int"),
    "a - b");
  CHECK_STRING_VALUE (
    make_test_of_binary_op (ctxt,
			    int_type,
			    GCC_JIT_BINARY_OP_MULT,
			    "test_BINARY_OP_MULT_on_int"),
    "a * b");
  CHECK_STRING_VALUE (
    make_test_of_binary_op (ctxt,
			    int_type,
			    GCC_JIT_BINARY_OP_DIVIDE,
			    "test_BINARY_OP_DIVIDE_on_int"),
    "a / b");
  /* TODO: test for DIVIDE on float or double */
  CHECK_STRING_VALUE (
    make_test_of_binary_op (ctxt,
			    int_type,
			    GCC_JIT_BINARY_OP_MODULO,
			    "test_BINARY_OP_MODULO_on_int"),
    "a % b");
  CHECK_STRING_VALUE (
    make_test_of_binary_op (ctxt,
			    int_type,
			    GCC_JIT_BINARY_OP_BITWISE_AND,
			    "test_BINARY_OP_BITWISE_AND_on_int"),
    "a & b");
  CHECK_STRING_VALUE (
    make_test_of_binary_op (ctxt,
			    int_type,
			    GCC_JIT_BINARY_OP_BITWISE_XOR,
			    "test_BINARY_OP_BITWISE_XOR_on_int"),
    "a ^ b");
  CHECK_STRING_VALUE (
    make_test_of_binary_op (ctxt,
			    int_type,
			    GCC_JIT_BINARY_OP_BITWISE_OR,
			    "test_BINARY_OP_BITWISE_OR_on_int"),
    "a | b");
  CHECK_STRING_VALUE (
    make_test_of_binary_op (ctxt,
			    int_type,
			    GCC_JIT_BINARY_OP_LOGICAL_AND,
			    "test_BINARY_OP_LOGICAL_AND_on_int"),
    "a && b");
  CHECK_STRING_VALUE (
    make_test_of_binary_op (ctxt,
			    int_type,
			    GCC_JIT_BINARY_OP_LOGICAL_OR,
			    "test_BINARY_OP_LOGICAL_OR_on_int"),
    "a || b");
  CHECK_STRING_VALUE (
    make_test_of_binary_op (ctxt,
			    int_type,
			    GCC_JIT_BINARY_OP_LSHIFT,
			    "test_BINARY_OP_LSHIFT_on_int"),
    "a << b");
  CHECK_STRING_VALUE (
    make_test_of_binary_op (ctxt,
			    int_type,
			    GCC_JIT_BINARY_OP_RSHIFT,
			    "test_BINARY_OP_RSHIFT_on_int"),
    "a >> b");
}

static void
verify_binary_ops (gcc_jit_result *result)
{
  typedef int (*test_fn) (int, int);

  test_fn test_BINARY_OP_PLUS_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_BINARY_OP_PLUS_on_int");
  CHECK_NON_NULL (test_BINARY_OP_PLUS_on_int);
  CHECK_VALUE (test_BINARY_OP_PLUS_on_int (0, 0), 0);
  CHECK_VALUE (test_BINARY_OP_PLUS_on_int (1, 2), 3);
  CHECK_VALUE (test_BINARY_OP_PLUS_on_int (100, -1), 99);
  CHECK_VALUE (test_BINARY_OP_PLUS_on_int (-1, -4), -5);

  test_fn test_BINARY_OP_MINUS_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_BINARY_OP_MINUS_on_int");
  CHECK_NON_NULL (test_BINARY_OP_MINUS_on_int);
  CHECK_VALUE (test_BINARY_OP_MINUS_on_int (0, 0), 0);
  CHECK_VALUE (test_BINARY_OP_MINUS_on_int (1, 2), -1);
  CHECK_VALUE (test_BINARY_OP_MINUS_on_int (100, -1), 101);
  CHECK_VALUE (test_BINARY_OP_MINUS_on_int (-1, -4), 3);

  test_fn test_BINARY_OP_MULT_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_BINARY_OP_MULT_on_int");
  CHECK_NON_NULL (test_BINARY_OP_MULT_on_int);
  CHECK_VALUE (test_BINARY_OP_MULT_on_int (0, 0), 0);
  CHECK_VALUE (test_BINARY_OP_MULT_on_int (1, 2), 2);
  CHECK_VALUE (test_BINARY_OP_MULT_on_int (100, -1), -100);
  CHECK_VALUE (test_BINARY_OP_MULT_on_int (-1, -4), 4);
  CHECK_VALUE (test_BINARY_OP_MULT_on_int (7, 10), 70);

  test_fn test_BINARY_OP_DIVIDE_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_BINARY_OP_DIVIDE_on_int");
  CHECK_NON_NULL (test_BINARY_OP_DIVIDE_on_int);
  CHECK_VALUE (test_BINARY_OP_DIVIDE_on_int (7, 2), 3);
  CHECK_VALUE (test_BINARY_OP_DIVIDE_on_int (100, -1), (100 / -1));
  CHECK_VALUE (test_BINARY_OP_DIVIDE_on_int (-1, -4), (-1 / -4));
  CHECK_VALUE (test_BINARY_OP_DIVIDE_on_int (60, 5), 12);

  /* TODO: test for DIVIDE on float or double */

  test_fn test_BINARY_OP_MODULO_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_BINARY_OP_MODULO_on_int");
  CHECK_NON_NULL (test_BINARY_OP_MODULO_on_int);
  CHECK_VALUE (test_BINARY_OP_MODULO_on_int (7, 2), 1);
  CHECK_VALUE (test_BINARY_OP_MODULO_on_int (100, -1), (100 % -1));
  CHECK_VALUE (test_BINARY_OP_MODULO_on_int (-1, -4), (-1 % -4));
  CHECK_VALUE (test_BINARY_OP_MODULO_on_int (60, 5), 0);

  test_fn test_BINARY_OP_BITWISE_AND_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_BINARY_OP_BITWISE_AND_on_int");
  CHECK_NON_NULL (test_BINARY_OP_BITWISE_AND_on_int);
  CHECK_VALUE (test_BINARY_OP_BITWISE_AND_on_int (0xf0f0, 0x7777), 0x7070);

  test_fn test_BINARY_OP_BITWISE_XOR_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_BINARY_OP_BITWISE_XOR_on_int");
  CHECK_NON_NULL (test_BINARY_OP_BITWISE_XOR_on_int);
  CHECK_VALUE (test_BINARY_OP_BITWISE_XOR_on_int (0xf0f0, 0x7777), 0x8787);

  test_fn test_BINARY_OP_BITWISE_OR_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_BINARY_OP_BITWISE_OR_on_int");
  CHECK_NON_NULL (test_BINARY_OP_BITWISE_OR_on_int);
  CHECK_VALUE (test_BINARY_OP_BITWISE_OR_on_int (0xf0f0, 0x7777), 0xf7f7);

  test_fn test_BINARY_OP_LOGICAL_AND_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_BINARY_OP_LOGICAL_AND_on_int");
  CHECK_NON_NULL (test_BINARY_OP_LOGICAL_AND_on_int);
  CHECK_VALUE (test_BINARY_OP_LOGICAL_AND_on_int (0, 0), 0);
  CHECK_VALUE (test_BINARY_OP_LOGICAL_AND_on_int (42, 0), 0);
  CHECK_VALUE (test_BINARY_OP_LOGICAL_AND_on_int (0, -13), 0);
  CHECK_VALUE (test_BINARY_OP_LOGICAL_AND_on_int (1997, 1998), 1);

  test_fn test_BINARY_OP_LOGICAL_OR_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_BINARY_OP_LOGICAL_OR_on_int");
  CHECK_NON_NULL (test_BINARY_OP_LOGICAL_OR_on_int);
  CHECK_VALUE (test_BINARY_OP_LOGICAL_OR_on_int (0, 0), 0);
  CHECK_VALUE (test_BINARY_OP_LOGICAL_OR_on_int (42, 0), 1);
  CHECK_VALUE (test_BINARY_OP_LOGICAL_OR_on_int (0, -13), 1);
  CHECK_VALUE (test_BINARY_OP_LOGICAL_OR_on_int (1997, 1998), 1);

  test_fn test_BINARY_OP_LSHIFT_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_BINARY_OP_LSHIFT_on_int");
  CHECK_NON_NULL (test_BINARY_OP_LSHIFT_on_int);
  CHECK_VALUE (test_BINARY_OP_LSHIFT_on_int (0, 0), 0);
  CHECK_VALUE (test_BINARY_OP_LSHIFT_on_int (0, 1), 0);
  CHECK_VALUE (test_BINARY_OP_LSHIFT_on_int (0, 2), 0);
  CHECK_VALUE (test_BINARY_OP_LSHIFT_on_int (1, 0), 1);
  CHECK_VALUE (test_BINARY_OP_LSHIFT_on_int (1, 1), 2);
  CHECK_VALUE (test_BINARY_OP_LSHIFT_on_int (1, 2), 4);
  CHECK_VALUE (test_BINARY_OP_LSHIFT_on_int (1, 3), 8);
  CHECK_VALUE (test_BINARY_OP_LSHIFT_on_int (3, 0), 3);
  CHECK_VALUE (test_BINARY_OP_LSHIFT_on_int (3, 1), 6);
  CHECK_VALUE (test_BINARY_OP_LSHIFT_on_int (3, 5), 3 * 32);
  CHECK_VALUE (test_BINARY_OP_LSHIFT_on_int (42, 0), 42);
  CHECK_VALUE (test_BINARY_OP_LSHIFT_on_int (42, 1), 84);

  test_fn test_BINARY_OP_RSHIFT_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_BINARY_OP_RSHIFT_on_int");
  CHECK_NON_NULL (test_BINARY_OP_RSHIFT_on_int);
  CHECK_VALUE (test_BINARY_OP_RSHIFT_on_int (0, 0), 0);
  CHECK_VALUE (test_BINARY_OP_RSHIFT_on_int (42, 0), 42);
  CHECK_VALUE (test_BINARY_OP_RSHIFT_on_int (42, 1), 21);
  CHECK_VALUE (test_BINARY_OP_RSHIFT_on_int (42, 2), 10);
}

/**********************************************************************
 Comparisons
 **********************************************************************/

static const char *
make_test_of_comparison (gcc_jit_context *ctxt,
			 gcc_jit_type *type,
			 enum gcc_jit_comparison op,
			 const char *funcname)
{
  /* Make a test function of the form:
       bool test_comparison_op (T a, T b)
       {
	  return a OP b;
       }
  */
  gcc_jit_param *param_a =
    gcc_jit_context_new_param (ctxt, NULL, type, "a");
  gcc_jit_param *param_b =
    gcc_jit_context_new_param (ctxt, NULL, type, "b");
  gcc_jit_param *params[] = {param_a, param_b};

  gcc_jit_rvalue *comparison =
    gcc_jit_context_new_comparison (
      ctxt,
      NULL,
      op,
      gcc_jit_param_as_rvalue (param_a),
      gcc_jit_param_as_rvalue (param_b));

  gcc_jit_type *comparison_type = gcc_jit_rvalue_get_type(comparison);

  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  comparison_type,
				  funcname,
				  2, params,
				  0);

  gcc_jit_block *initial = gcc_jit_function_new_block (test_fn, "initial");
  gcc_jit_block_end_with_return (initial, NULL, comparison);

  return gcc_jit_object_get_debug_string (
    gcc_jit_rvalue_as_object (comparison));
}

static void run_test_of_comparison(gcc_jit_context *ctxt,
			 gcc_jit_type *type,
			 enum gcc_jit_comparison op,
			 const char *funcname,
			 const char *vec_funcname,
			 const char *expected)
{
  gcc_jit_type *vec_type =
    gcc_jit_type_get_vector (type, 4);

  CHECK_STRING_VALUE (
    make_test_of_comparison (ctxt,
			     type,
			     op,
			     funcname),
    expected);
  CHECK_STRING_VALUE (
    make_test_of_comparison (ctxt,
			     vec_type,
			     op,
			     vec_funcname),
    expected);
}

static void
make_tests_of_comparisons (gcc_jit_context *ctxt)
{
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *float_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_FLOAT);

  run_test_of_comparison(
  	ctxt,
  	int_type,
  	GCC_JIT_COMPARISON_EQ,
  	"test_COMPARISON_EQ_on_int",
  	"test_COMPARISON_EQ_on_vec_int",
  	"a == b");
  run_test_of_comparison(
  	ctxt,
  	int_type,
  	GCC_JIT_COMPARISON_NE,
  	"test_COMPARISON_NE_on_int",
  	"test_COMPARISON_NE_on_vec_int",
  	"a != b");
  run_test_of_comparison(
  	ctxt,
  	int_type,
  	GCC_JIT_COMPARISON_LT,
  	"test_COMPARISON_LT_on_int",
  	"test_COMPARISON_LT_on_vec_int",
  	"a < b");
  run_test_of_comparison(
  	ctxt,
  	int_type,
  	GCC_JIT_COMPARISON_LE,
  	"test_COMPARISON_LE_on_int",
  	"test_COMPARISON_LE_on_vec_int",
  	"a <= b");
  run_test_of_comparison(
  	ctxt,
  	int_type,
  	GCC_JIT_COMPARISON_GT,
  	"test_COMPARISON_GT_on_int",
  	"test_COMPARISON_GT_on_vec_int",
  	"a > b");
  run_test_of_comparison(
  	ctxt,
  	int_type,
  	GCC_JIT_COMPARISON_GE,
  	"test_COMPARISON_GE_on_int",
  	"test_COMPARISON_GE_on_vec_int",
  	"a >= b");

  // Float tests
  run_test_of_comparison(
  	ctxt,
  	float_type,
  	GCC_JIT_COMPARISON_NE,
  	"test_COMPARISON_NE_on_float",
  	"test_COMPARISON_NE_on_vec_float",
  	"a != b");
  run_test_of_comparison(
  	ctxt,
  	float_type,
  	GCC_JIT_COMPARISON_LT,
  	"test_COMPARISON_LT_on_float",
  	"test_COMPARISON_LT_on_vec_float",
  	"a < b");
  run_test_of_comparison(
  	ctxt,
  	float_type,
  	GCC_JIT_COMPARISON_GT,
  	"test_COMPARISON_GT_on_float",
  	"test_COMPARISON_GT_on_vec_float",
  	"a > b");
}

static void
verify_comparisons (gcc_jit_result *result)
{
  typedef bool (*test_fn) (int, int);

  test_fn test_COMPARISON_EQ_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_COMPARISON_EQ_on_int");
  CHECK_NON_NULL (test_COMPARISON_EQ_on_int);
  CHECK_VALUE (test_COMPARISON_EQ_on_int (0, 0), 1);
  CHECK_VALUE (test_COMPARISON_EQ_on_int (1, 2), 0);

  test_fn test_COMPARISON_NE_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_COMPARISON_NE_on_int");
  CHECK_NON_NULL (test_COMPARISON_NE_on_int);
  CHECK_VALUE (test_COMPARISON_NE_on_int (0, 0), 0);
  CHECK_VALUE (test_COMPARISON_NE_on_int (1, 2), 1);

  test_fn test_COMPARISON_LT_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_COMPARISON_LT_on_int");
  CHECK_NON_NULL (test_COMPARISON_LT_on_int);
  CHECK_VALUE (test_COMPARISON_LT_on_int (0, 0), 0);
  CHECK_VALUE (test_COMPARISON_LT_on_int (1, 2), 1);
  CHECK_VALUE (test_COMPARISON_LT_on_int (2, 1), 0);
  CHECK_VALUE (test_COMPARISON_LT_on_int (-2, 1), 1);

  test_fn test_COMPARISON_LE_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_COMPARISON_LE_on_int");
  CHECK_NON_NULL (test_COMPARISON_LE_on_int);
  CHECK_VALUE (test_COMPARISON_LE_on_int (0, 0), 1);
  CHECK_VALUE (test_COMPARISON_LE_on_int (1, 2), 1);
  CHECK_VALUE (test_COMPARISON_LE_on_int (2, 1), 0);

  test_fn test_COMPARISON_GT_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_COMPARISON_GT_on_int");
  CHECK_NON_NULL (test_COMPARISON_GT_on_int);
  CHECK_VALUE (test_COMPARISON_GT_on_int (0, 0), 0);
  CHECK_VALUE (test_COMPARISON_GT_on_int (1, 2), 0);
  CHECK_VALUE (test_COMPARISON_GT_on_int (2, 1), 1);

  test_fn test_COMPARISON_GE_on_int =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_COMPARISON_GE_on_int");
  CHECK_NON_NULL (test_COMPARISON_GE_on_int);
  CHECK_VALUE (test_COMPARISON_GE_on_int (0, 0), 1);
  CHECK_VALUE (test_COMPARISON_GE_on_int (1, 2), 0);
  CHECK_VALUE (test_COMPARISON_GE_on_int (2, 1), 1);

  typedef int __vector __attribute__ ((__vector_size__ (sizeof(int) * 2)));
  typedef __vector (*test_vec_fn) (__vector, __vector);

  __vector zero_zero = {0, 0};
  __vector zero_one = {0, 1};
  __vector one_zero = {1, 0};

  __vector true_true = {-1, -1};
  __vector false_true = {0, -1};
  __vector true_false = {-1, 0};
  __vector false_false = {0, 0};

  test_vec_fn test_COMPARISON_EQ_on_vec_int =
    (test_vec_fn)gcc_jit_result_get_code (result,
				      "test_COMPARISON_EQ_on_vec_int");
  CHECK_NON_NULL (test_COMPARISON_EQ_on_vec_int);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_EQ_on_vec_int (zero_zero, zero_zero), true_true);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_EQ_on_vec_int (zero_one, one_zero), false_false);

  test_vec_fn test_COMPARISON_NE_on_vec_int =
    (test_vec_fn)gcc_jit_result_get_code (result,
				      "test_COMPARISON_NE_on_vec_int");
  CHECK_NON_NULL (test_COMPARISON_NE_on_vec_int);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_NE_on_vec_int (zero_zero, zero_zero), false_false);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_NE_on_vec_int (zero_one, one_zero), true_true);

  test_vec_fn test_COMPARISON_LT_on_vec_int =
    (test_vec_fn)gcc_jit_result_get_code (result,
				      "test_COMPARISON_LT_on_vec_int");
  CHECK_NON_NULL (test_COMPARISON_LT_on_vec_int);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_LT_on_vec_int (zero_zero, zero_zero), false_false);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_LT_on_vec_int (zero_one, one_zero), true_false);

  test_vec_fn test_COMPARISON_LE_on_vec_int =
    (test_vec_fn)gcc_jit_result_get_code (result,
				      "test_COMPARISON_LE_on_vec_int");
  CHECK_NON_NULL (test_COMPARISON_LE_on_vec_int);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_LE_on_vec_int (zero_zero, zero_zero), true_true);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_LE_on_vec_int (zero_one, one_zero), true_false);

  test_vec_fn test_COMPARISON_GT_on_vec_int =
    (test_vec_fn)gcc_jit_result_get_code (result,
				      "test_COMPARISON_GT_on_vec_int");
  CHECK_NON_NULL (test_COMPARISON_GT_on_vec_int);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_GT_on_vec_int (zero_zero, zero_zero), false_false);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_GT_on_vec_int (zero_one, one_zero), false_true);

  test_vec_fn test_COMPARISON_GE_on_vec_int =
    (test_vec_fn)gcc_jit_result_get_code (result,
				      "test_COMPARISON_GE_on_vec_int");
  CHECK_NON_NULL (test_COMPARISON_GE_on_vec_int);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_GE_on_vec_int (zero_zero, zero_zero), true_true);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_GE_on_vec_int (zero_one, one_zero), false_true);

  typedef float __vector_f __attribute__ ((__vector_size__ (sizeof(float) * 2)));
  typedef __vector (*test_vec_f_fn) (__vector_f, __vector_f);

  __vector_f zero_zero_f = {0, 0};
  __vector_f zero_one_f = {0, 1};
  __vector_f one_zero_f = {1, 0};

  __vector_f true_true_f = {-1, -1};
  __vector_f false_true_f = {0, -1};
  __vector_f true_false_f = {-1, 0};
  __vector_f false_false_f = {0, 0};

  test_vec_f_fn test_COMPARISON_NE_on_vec_float =
    (test_vec_f_fn)gcc_jit_result_get_code (result,
				      "test_COMPARISON_NE_on_vec_float");
  CHECK_NON_NULL (test_COMPARISON_NE_on_vec_float);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_NE_on_vec_float (zero_zero_f, zero_zero_f), false_false_f);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_NE_on_vec_float (zero_one_f, one_zero_f), true_true_f);

  test_vec_f_fn test_COMPARISON_LT_on_vec_float =
    (test_vec_f_fn)gcc_jit_result_get_code (result,
				      "test_COMPARISON_LT_on_vec_float");
  CHECK_NON_NULL (test_COMPARISON_LT_on_vec_float);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_LT_on_vec_float (zero_zero_f, zero_zero_f), false_false_f);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_LT_on_vec_float (zero_one_f, one_zero_f), true_false_f);

  test_vec_f_fn test_COMPARISON_GT_on_vec_float =
    (test_vec_f_fn)gcc_jit_result_get_code (result,
				      "test_COMPARISON_GT_on_vec_float");
  CHECK_NON_NULL (test_COMPARISON_GT_on_vec_float);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_GT_on_vec_float (zero_zero_f, zero_zero_f), false_false_f);
  CHECK_VECTOR_VALUE (2, test_COMPARISON_GT_on_vec_float (zero_one_f, one_zero_f), false_true_f);
}

/**********************************************************************
 Casts
 **********************************************************************/

static const char*
make_test_of_cast (gcc_jit_context *ctxt,
		   gcc_jit_type *input_type,
		   gcc_jit_type *output_type,
		   const char *funcname)
{
  /* Make a test function of the form:
       OUTPUT_TYPE test_cast_* (INPUT_TYPE a)
       {
          return (OUTPUT_TYPE)a;
       }
  */
  gcc_jit_param *param_a =
    gcc_jit_context_new_param (ctxt, NULL, input_type, "a");
  gcc_jit_param *params[] = {param_a};
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  output_type,
				  funcname,
				  1, params,
				  0);
  gcc_jit_rvalue *cast =
    gcc_jit_context_new_cast (
      ctxt,
      NULL,
      gcc_jit_param_as_rvalue (param_a),
      output_type);
  gcc_jit_block *initial = gcc_jit_function_new_block (test_fn, "initial");
  gcc_jit_block_end_with_return (initial, NULL, cast);

  return gcc_jit_object_get_debug_string (
    gcc_jit_rvalue_as_object (cast));
}

/* For use by test_cast_from_array_of_ints_to_int_ptr.  */
extern int called_pointer_checking_function (int *ints)
{
  CHECK_VALUE (ints[0], 10);
  CHECK_VALUE (ints[1], 4);
  return ints[0] * ints[1];
}

static void
make_tests_of_casts (gcc_jit_context *ctxt)
{
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *long_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_LONG);
  gcc_jit_type *float_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_FLOAT);
  gcc_jit_type *bool_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_BOOL);
  gcc_jit_type *array_int_type =
    gcc_jit_context_new_array_type (ctxt, NULL,
				    int_type,
				    2);
  gcc_jit_type *int_ptr_type =
    gcc_jit_type_get_pointer (int_type);

  /* float/int conversions */
  CHECK_STRING_VALUE (
    make_test_of_cast (ctxt,
		       float_type,
		       int_type,
		       "test_cast_from_float_to_int"),
    "(int)a");
  CHECK_STRING_VALUE (
    make_test_of_cast (ctxt,
		       int_type,
		       float_type,
		       "test_cast_from_int_to_float"),
    "(float)a");

  /* bool/int conversions */
  CHECK_STRING_VALUE (
    make_test_of_cast (ctxt,
		       bool_type,
		       int_type,
		       "test_cast_from_bool_to_int"),
    "(int)a");
  CHECK_STRING_VALUE (
    make_test_of_cast (ctxt,
		       int_type,
		       bool_type,
		       "test_cast_from_int_to_bool"),
    "(bool)a");

  /* bool/long conversions */
  CHECK_STRING_VALUE (
    make_test_of_cast (ctxt,
		       bool_type,
		       long_type,
		       "test_cast_from_bool_to_long"),
    "(long)a");
  CHECK_STRING_VALUE (
    make_test_of_cast (ctxt,
		       long_type,
		       bool_type,
		       "test_cast_from_long_to_bool"),
    "(bool)a");

  /* array/ptr conversions */
  {
    gcc_jit_function *test_fn =
      gcc_jit_context_new_function (
	ctxt, NULL,
	GCC_JIT_FUNCTION_EXPORTED,
	int_type,
	"test_cast_from_array_of_ints_to_int_ptr",
	0, NULL,
	0);
    /* Equivalent to:
          int test_cast_from_array_of_ints_to_int_ptr (void)
	  {
	    int array[2];
	    array[0] = 10;
	    array[1] = 4;
	    return called_pointer_checking_function (array);
	  }
    */

    gcc_jit_param *param_ints =
      gcc_jit_context_new_param (ctxt, NULL, int_ptr_type, "ints");
    gcc_jit_function *called_fn =
      gcc_jit_context_new_function (
	ctxt, NULL,
	GCC_JIT_FUNCTION_IMPORTED,
	int_type,
	"called_pointer_checking_function",
	1, &param_ints,
	0);

    gcc_jit_lvalue *array =
      gcc_jit_function_new_local (test_fn, NULL,
				  array_int_type,
				  "array");
    gcc_jit_block *block =
      gcc_jit_function_new_block (test_fn, "block");
    /* array[0] = 10; */
    gcc_jit_block_add_assignment (
      block, NULL,
      gcc_jit_context_new_array_access (
	ctxt, NULL,
	gcc_jit_lvalue_as_rvalue (array),
	gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 0)),
      gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 10));
    /* array[1] = 4; */
    gcc_jit_block_add_assignment (
      block, NULL,
      gcc_jit_context_new_array_access (
	ctxt, NULL,
	gcc_jit_lvalue_as_rvalue (array),
	gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 1)),
      gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 4));
    gcc_jit_rvalue *cast =
      gcc_jit_context_new_cast (
	ctxt,
	NULL,
	/* We need a get_address here.  */
	gcc_jit_lvalue_get_address (array, NULL),
	int_ptr_type);
    gcc_jit_block_end_with_return (
      block, NULL,
      gcc_jit_context_new_call (
	ctxt, NULL,
	called_fn,
	1, &cast));

    CHECK_STRING_VALUE (
      gcc_jit_object_get_debug_string (
	gcc_jit_rvalue_as_object (cast)),
      "(int *)&array");
  }
}

static void
verify_casts (gcc_jit_result *result)
{
  /* float to int */
  {
    typedef int (*fn_type) (float);
    fn_type test_cast_from_float_to_int =
      (fn_type)gcc_jit_result_get_code (result,
					"test_cast_from_float_to_int");
    CHECK_NON_NULL (test_cast_from_float_to_int);
    CHECK_VALUE (test_cast_from_float_to_int (4.2), 4);
  }

  /* int to float */
  {
    typedef float (*fn_type) (int);
    fn_type test_cast_from_int_to_float =
      (fn_type)gcc_jit_result_get_code (result,
					"test_cast_from_int_to_float");
    CHECK_NON_NULL (test_cast_from_int_to_float);
    CHECK_VALUE (test_cast_from_int_to_float (4), 4.0);
  }

  /* bool to int */
  {
    typedef int (*fn_type) (bool);
    fn_type test_cast_from_bool_to_int =
      (fn_type)gcc_jit_result_get_code (result,
					"test_cast_from_bool_to_int");
    CHECK_NON_NULL (test_cast_from_bool_to_int);
    CHECK_VALUE (test_cast_from_bool_to_int (0), 0);
    CHECK_VALUE (test_cast_from_bool_to_int (1), 1);
  }

  /* int to bool */
  {
    typedef bool (*fn_type) (int);
    fn_type test_cast_from_int_to_bool =
      (fn_type)gcc_jit_result_get_code (result,
					"test_cast_from_int_to_bool");
    CHECK_NON_NULL (test_cast_from_int_to_bool);
    CHECK_VALUE (test_cast_from_int_to_bool (0), 0);
    CHECK_VALUE (test_cast_from_int_to_bool (1), 1);
  }

  /* bool to long */
  {
    typedef long (*fn_type) (bool);
    fn_type test_cast_from_bool_to_long =
      (fn_type)gcc_jit_result_get_code (result,
					"test_cast_from_bool_to_long");
    CHECK_NON_NULL (test_cast_from_bool_to_long);
    CHECK_VALUE (test_cast_from_bool_to_long (0), 0);
    CHECK_VALUE (test_cast_from_bool_to_long (1), 1);
  }

  /* long to bool */
  {
    typedef bool (*fn_type) (long);
    fn_type test_cast_from_long_to_bool =
      (fn_type)gcc_jit_result_get_code (result,
					"test_cast_from_long_to_bool");
    CHECK_NON_NULL (test_cast_from_long_to_bool);
    CHECK_VALUE (test_cast_from_long_to_bool (0), 0);
    CHECK_VALUE (test_cast_from_long_to_bool (1), 1);
  }

  /* array to ptr */
  {
    typedef int (*fn_type) (void);
    fn_type test_cast_from_array_of_ints_to_int_ptr =
      (fn_type)gcc_jit_result_get_code (
	result,
	"test_cast_from_array_of_ints_to_int_ptr");
    CHECK_NON_NULL (test_cast_from_array_of_ints_to_int_ptr);
    CHECK_VALUE (test_cast_from_array_of_ints_to_int_ptr (), 40);
  }
}

/**********************************************************************
 Dereferences
 **********************************************************************/

static void
make_tests_of_dereferences (gcc_jit_context *ctxt)
{
  /*
       int test_dereference_read (int *ptr)
       {
	 return *ptr;
       }
       void test_dereference_write (int *ptr, int i)
       {
	 *ptr = i;
       }
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *int_ptr_type =
    gcc_jit_type_get_pointer (int_type);
  {
    gcc_jit_param *param_ptr =
      gcc_jit_context_new_param (ctxt, NULL, int_ptr_type, "ptr");
    gcc_jit_function *test_dereference_read =
      gcc_jit_context_new_function (ctxt, NULL,
				    GCC_JIT_FUNCTION_EXPORTED,
				    int_type,
				    "test_dereference_read",
				    1, &param_ptr,
				    0);
    gcc_jit_block *initial =
      gcc_jit_function_new_block (test_dereference_read, "initial");
    gcc_jit_block_end_with_return (
      initial,
      NULL,
      gcc_jit_lvalue_as_rvalue (
	gcc_jit_rvalue_dereference (
	  gcc_jit_param_as_rvalue (param_ptr),
	  NULL)));
  }

  {
    gcc_jit_param *param_ptr =
      gcc_jit_context_new_param (ctxt, NULL, int_ptr_type, "ptr");
    gcc_jit_param *param_i =
      gcc_jit_context_new_param (ctxt, NULL, int_type, "i");
    gcc_jit_param *params[] = {param_ptr, param_i};
    gcc_jit_function *test_dereference_write =
      gcc_jit_context_new_function (ctxt, NULL,
				    GCC_JIT_FUNCTION_EXPORTED,
				    void_type,
				    "test_dereference_write",
				    2, params,
				    0);
    gcc_jit_block *initial =
      gcc_jit_function_new_block (test_dereference_write, "initial");
    gcc_jit_block_add_assignment (
      initial,
      NULL,
      gcc_jit_rvalue_dereference (
	gcc_jit_param_as_rvalue (param_ptr),
	NULL),
      gcc_jit_param_as_rvalue (param_i));
    gcc_jit_block_end_with_void_return (initial, NULL);
  }
}

static void
verify_dereferences (gcc_jit_result *result)
{
  int a = 42;
  int b = -99;

  {
    typedef int (*test_read) (int *);
    test_read test_dereference_read =
      (test_read)gcc_jit_result_get_code (result,
					  "test_dereference_read");
    CHECK_NON_NULL (test_dereference_read);
    CHECK_VALUE (test_dereference_read (&a), 42);
    CHECK_VALUE (test_dereference_read (&b), -99);
  }

 {
    typedef void (*test_write) (int *, int);
    test_write test_dereference_write =
      (test_write)gcc_jit_result_get_code (result,
					  "test_dereference_write");
    CHECK_NON_NULL (test_dereference_write);
    test_dereference_write (&a, -55);
    CHECK_VALUE (a, -55);

    test_dereference_write (&b, 404);
    CHECK_VALUE (b, 404);
  }
}

/**********************************************************************
 gcc_jit_lvalue_get_address
 **********************************************************************/

int test_global;
static void
make_test_of_get_address (gcc_jit_context *ctxt)
{
  /*
     void *test_get_address (void)
     {
	return &test_global;
     }
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_lvalue *test_global =
    gcc_jit_context_new_global (
      ctxt,
      NULL,
      GCC_JIT_GLOBAL_IMPORTED,
      int_type,
      "test_global");

 gcc_jit_type *void_ptr_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID_PTR);

  gcc_jit_function *test_get_address =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  void_ptr_type,
				  "test_get_address",
				  0, NULL,
				  0);
  gcc_jit_block *initial =
    gcc_jit_function_new_block (test_get_address, "initial");
  gcc_jit_block_end_with_return (
    initial,
    NULL,
    gcc_jit_lvalue_get_address (
      test_global,
      NULL));
}

static void
verify_get_address (gcc_jit_result *result)
{
  typedef void *(*test_fn) (void);
    test_fn test_get_address =
      (test_fn)gcc_jit_result_get_code (result,
					"test_get_address");
  CHECK_NON_NULL (test_get_address);
  CHECK_VALUE (test_get_address (), &test_global);
}

/**********************************************************************
 Vector values
 **********************************************************************/

static void
make_test_of_vectors (gcc_jit_context *ctxt)
{
  gcc_jit_type *scalar_type;
  gcc_jit_type *vec_type;
  gcc_jit_rvalue *elements[4];

  scalar_type = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  vec_type = gcc_jit_type_get_vector (scalar_type, 4);

  elements[0] = gcc_jit_context_new_rvalue_from_int (ctxt, scalar_type, 1);
  elements[1] = gcc_jit_context_new_rvalue_from_int (ctxt, scalar_type, -2);
  elements[2] = gcc_jit_context_new_rvalue_from_int (ctxt, scalar_type, 3);
  elements[3] = gcc_jit_context_new_rvalue_from_int (ctxt, scalar_type, -4);

  gcc_jit_rvalue *vec_rvalue
    = gcc_jit_context_new_rvalue_from_vector (ctxt, NULL, vec_type,
					      4, elements);
  CHECK_STRING_VALUE (
    gcc_jit_object_get_debug_string (
      gcc_jit_rvalue_as_object (vec_rvalue)),
    "{(int)1, (int)-2, (int)3, (int)-4}");
}

/**********************************************************************
 Code for harness
 **********************************************************************/

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  make_tests_of_unary_ops (ctxt);
  make_tests_of_binary_ops (ctxt);
  make_tests_of_comparisons (ctxt);
  make_tests_of_casts (ctxt);
  make_tests_of_dereferences (ctxt);
  make_test_of_get_address (ctxt);
  make_test_of_vectors (ctxt);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);

  verify_unary_ops (result);
  verify_binary_ops (result);
  verify_comparisons (result);
  verify_casts (result);
  verify_dereferences (result);
  verify_get_address (result);
}
