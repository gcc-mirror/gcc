#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

/**********************************************************************
 GCC_JIT_FUNCTION_ALWAYS_INLINE and GCC_JIT_FUNCTION_INTERNAL
 **********************************************************************/
static void
create_test_of_hidden_function (gcc_jit_context *ctxt,
				enum gcc_jit_function_kind hidden_kind,
				const char *hidden_func_name,
				const char *visible_func_name)
{
  /* Let's try to inject the equivalent of:
     static double hidden_mult (double a, double b)
     {
       return x * x;
     }
     double my_square (double x)
     {
       return my_mult (x, x);
     }

     where hidden_mult can potentially be
       inline  __attribute__((always_inline)).  */
  gcc_jit_type *double_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_DOUBLE);

  /* Create "my_mult" */
  gcc_jit_param *param_a =
    gcc_jit_context_new_param (ctxt, NULL, double_type, "a");
  gcc_jit_param *param_b =
    gcc_jit_context_new_param (ctxt, NULL, double_type, "b");
  gcc_jit_param *params[2] = {param_a, param_b};
  gcc_jit_function *my_mult =
    gcc_jit_context_new_function (ctxt, NULL,
				  hidden_kind,
                                  double_type,
                                  hidden_func_name,
                                  2, params,
                                  0);
  gcc_jit_block *body_of_my_mult =
    gcc_jit_function_new_block (my_mult, NULL);
  gcc_jit_block_end_with_return (
    body_of_my_mult, NULL,
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      GCC_JIT_BINARY_OP_MULT,
      double_type,
      gcc_jit_param_as_rvalue (param_a),
      gcc_jit_param_as_rvalue (param_b)));

  /* Create "my_square" */
  gcc_jit_param *param_x =
    gcc_jit_context_new_param (ctxt, NULL, double_type, "x");
  gcc_jit_function *my_square =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  double_type,
                                  visible_func_name,
                                  1, &param_x,
                                  0);
  gcc_jit_block *body_of_my_square =
    gcc_jit_function_new_block (my_square, NULL);
  gcc_jit_rvalue *args[2] = {gcc_jit_param_as_rvalue (param_x),
			     gcc_jit_param_as_rvalue (param_x)};
  gcc_jit_block_end_with_return (
    body_of_my_square, NULL,
    gcc_jit_context_new_call (
      ctxt, NULL,
      my_mult,
      2, args));
}

static void
create_tests_of_hidden_functions (gcc_jit_context *ctxt)
{
  create_test_of_hidden_function (ctxt,
				  GCC_JIT_FUNCTION_INTERNAL,
				  "my_internal_mult",
				  "my_square_with_internal");
  create_test_of_hidden_function (ctxt,
				  GCC_JIT_FUNCTION_ALWAYS_INLINE,
				  "my_always_inline_mult",
				  "my_square_with_always_inline");
}

static void
verify_hidden_functions (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);

  /* GCC_JIT_FUNCTION_INTERNAL and GCC_JIT_FUNCTION_ALWAYS_INLINE
     functions should not be accessible in the result.  */
  CHECK_VALUE (NULL, gcc_jit_result_get_code (result, "my_internal_mult"));
  CHECK_VALUE (NULL, gcc_jit_result_get_code (result, "my_always_inline_mult"));

  typedef double (*fn_type) (double);
  fn_type my_square_with_internal =
    (fn_type)gcc_jit_result_get_code (result, "my_square_with_internal");
  CHECK_NON_NULL (my_square_with_internal);
  CHECK_VALUE (my_square_with_internal (5.0), 25.0);

  fn_type my_square_with_always_inline =
    (fn_type)gcc_jit_result_get_code (result, "my_square_with_always_inline");
  CHECK_NON_NULL (my_square_with_always_inline);
  CHECK_VALUE (my_square_with_always_inline (5.0), 25.0);
}

/**********************************************************************
 Builtin functions
 **********************************************************************/

static void
create_test_of_builtin_strcmp (gcc_jit_context *ctxt)
{
  /* Let's try to inject the equivalent of:
       int
       test_of_builtin_strcmp (const char *a, const char *b)
       {
         return __builtin_strcmp (a, b);
       }
  */
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *const_char_ptr_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CONST_CHAR_PTR);

  /* Get the built-in function.  */
  gcc_jit_function *builtin_fn =
    gcc_jit_context_get_builtin_function (ctxt, "strcmp");

  CHECK_STRING_VALUE (
    gcc_jit_object_get_debug_string (gcc_jit_function_as_object (builtin_fn)),
    "strcmp");

  /* Build the test_fn.  */
  gcc_jit_param *param_a =
    gcc_jit_context_new_param (ctxt, NULL, const_char_ptr_type, "a");
  gcc_jit_param *param_b =
    gcc_jit_context_new_param (ctxt, NULL, const_char_ptr_type, "b");
  gcc_jit_param *params[2] = {param_a, param_b};
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  int_type,
                                  "test_of_builtin_strcmp",
                                  2, params,
                                  0);
  gcc_jit_rvalue *args[2] = {gcc_jit_param_as_rvalue (param_a),
			     gcc_jit_param_as_rvalue (param_b)};
  gcc_jit_rvalue *call =
    gcc_jit_context_new_call (ctxt,
                              NULL,
                              builtin_fn,
                              2, args);
  CHECK_STRING_VALUE (
    gcc_jit_object_get_debug_string (gcc_jit_rvalue_as_object (call)),
    "strcmp (a, b)");

  gcc_jit_block *initial =
    gcc_jit_function_new_block (test_fn, "initial");
  gcc_jit_block_end_with_return (initial, NULL, call);
}

static char *trig_sincos_dump;
static char *trig_statistics_dump;

static void
create_test_of_builtin_trig (gcc_jit_context *ctxt)
{
  /* Let's try to inject the equivalent of:
       int
       test_of_builtin_trig (double theta)
       {
         return 2 * sin (theta) * cos (theta);
       }
       (in theory, optimizable to sin (2 * theta))
  */

  gcc_jit_context_enable_dump (ctxt,
			       "tree-sincos",
			       &trig_sincos_dump);
  gcc_jit_context_enable_dump (ctxt,
			       "statistics",
			       &trig_statistics_dump);

  gcc_jit_type *double_t =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_DOUBLE);

  /* Get the built-in functions.  */
  gcc_jit_function *builtin_sin =
    gcc_jit_context_get_builtin_function (ctxt, "sin");
  gcc_jit_function *builtin_cos =
    gcc_jit_context_get_builtin_function (ctxt, "cos");

  /* Build the test_fn.  */
  gcc_jit_param *param_theta =
    gcc_jit_context_new_param (ctxt, NULL, double_t, "theta");
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  double_t,
                                  "test_of_builtin_trig",
                                  1, &param_theta,
                                  0);
  gcc_jit_rvalue *args[1] = {gcc_jit_param_as_rvalue (param_theta)};
  gcc_jit_rvalue *two =
    gcc_jit_context_new_rvalue_from_int (ctxt, double_t, 2);
  gcc_jit_rvalue *ret =
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      GCC_JIT_BINARY_OP_MULT,
      double_t,
      two,
      gcc_jit_context_new_binary_op (
        ctxt, NULL,
	GCC_JIT_BINARY_OP_MULT,
	double_t,
	gcc_jit_context_new_call (ctxt, NULL,
				  builtin_sin,
				  1, args),
	gcc_jit_context_new_call (ctxt, NULL,
				  builtin_cos,
				  1, args)));
  CHECK_STRING_VALUE (
    gcc_jit_object_get_debug_string (gcc_jit_rvalue_as_object (ret)),
    "(double)2 * sin (theta) * cos (theta)");

  gcc_jit_block *initial =
    gcc_jit_function_new_block (test_fn, "initial");
  gcc_jit_block_end_with_return (initial, NULL, ret);
}

static void
create_use_of_builtins (gcc_jit_context *ctxt)
{
  create_test_of_builtin_strcmp (ctxt);
  create_test_of_builtin_trig (ctxt);
}

static void
verify_test_of_builtin_strcmp (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*fn_type) (const char *, const char *);
  CHECK_NON_NULL (result);

  fn_type test_of_builtin_strcmp =
    (fn_type)gcc_jit_result_get_code (result, "test_of_builtin_strcmp");
  CHECK_NON_NULL (test_of_builtin_strcmp);

  /* Verify that it correctly called strcmp.  */
  CHECK_VALUE (test_of_builtin_strcmp ("foo", "foo"), 0);
  CHECK (test_of_builtin_strcmp ("foo", "bar") > 0);
  CHECK (test_of_builtin_strcmp ("bar", "foo") < 0);
}

static void
verify_test_of_builtin_trig (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef double (*fn_type) (double);
  CHECK_NON_NULL (result);

  fn_type test_of_builtin_trig =
    (fn_type)gcc_jit_result_get_code (result, "test_of_builtin_trig");
  CHECK_NON_NULL (test_of_builtin_trig);

  /* Verify that it correctly computes
        sin (2 * theta)
     (perhaps calling sin and cos). */
  CHECK_DOUBLE_VALUE (test_of_builtin_trig (0.0         ),  0.0);
  CHECK_DOUBLE_VALUE (test_of_builtin_trig (M_PI_4      ),  1.0);
  CHECK_DOUBLE_VALUE (test_of_builtin_trig (M_PI_2      ),  0.0);
  CHECK_DOUBLE_VALUE (test_of_builtin_trig (M_PI_4 * 3.0), -1.0);
  CHECK_DOUBLE_VALUE (test_of_builtin_trig (M_PI        ),  0.0);

  /* PR jit/64020:
     The "sincos" pass merges sin/cos calls into the cexpi builtin.
     Verify that a dump of the "sincos" pass was provided, and that it
     shows a call to the cexpi builtin on a SSA name of "theta".  */
  CHECK_NON_NULL (trig_sincos_dump);
  CHECK_STRING_CONTAINS (trig_sincos_dump, " = __builtin_cexpi (theta_");
  free (trig_sincos_dump);

  /* Similarly, verify that the statistics dump was provided, and that
     it shows the sincos optimization.  */
  CHECK_NON_NULL (trig_statistics_dump);
  CHECK_STRING_CONTAINS (
    trig_statistics_dump,
    "sincos \"sincos statements inserted\" \"test_of_builtin_trig\" 1");
  free (trig_statistics_dump);
}

static void
verify_use_of_builtins (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  verify_test_of_builtin_strcmp (ctxt, result);
  verify_test_of_builtin_trig (ctxt, result);
}

/**********************************************************************
 "void" return
 **********************************************************************/

static void
create_use_of_void_return (gcc_jit_context *ctxt)
{
  /* Let's try to inject the equivalent of:
       void
       test_of_void_return (int *out)
       {
         *out = 1;
	 return;
       }
  */
  gcc_jit_type *void_t =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_t =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *int_ptr_t =
    gcc_jit_type_get_pointer (int_t);

  /* Build the test_fn.  */
  gcc_jit_param *param_out =
    gcc_jit_context_new_param (ctxt, NULL, int_ptr_t, "out");
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  void_t,
                                  "test_of_void_return",
                                  1, &param_out,
                                  0);
  gcc_jit_block *initial =
    gcc_jit_function_new_block (test_fn, "initial");

  gcc_jit_block_add_assignment (
    initial, NULL,
    /* "*out = ..." */
    gcc_jit_rvalue_dereference (gcc_jit_param_as_rvalue (param_out),
				NULL),
    gcc_jit_context_one (ctxt, int_t));
  gcc_jit_block_end_with_void_return (initial, NULL);
}

static void
verify_void_return (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef void (*fn_type) (int *);
  CHECK_NON_NULL (result);

  fn_type test_of_void_return =
    (fn_type)gcc_jit_result_get_code (result, "test_of_void_return");
  CHECK_NON_NULL (test_of_void_return);

  int i;
  test_of_void_return (&i);
  CHECK_VALUE (i, 1); /* ensure correct value was written back */
}

/**********************************************************************
 Code for harness
 **********************************************************************/

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  create_tests_of_hidden_functions (ctxt);
  create_use_of_builtins (ctxt);
  create_use_of_void_return (ctxt);
}


void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  verify_hidden_functions (ctxt, result);
  verify_use_of_builtins (ctxt, result);
  verify_void_return (ctxt, result);
}
