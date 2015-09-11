#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

struct quadratic
{
  double a;
  double b;
  double c;
  double discriminant;
};

/* Let's try to inject the equivalent of:

     extern double sqrt (double);

     static void
     calc_discriminant (struct quadratic *q)
     {
       // (b^2 - 4ac)
       q->discriminant = (q->b * q->b) - (4 * q->a * q->c);
     }

     int
     test_quadratic (double a, double b, double c, double *r1, double *r2)
     {
       struct quadratic q;
       q.a = a;
       q.b = b;
       q.c = c;
       calc_discriminant (&q);
       if (q.discriminant > 0)
	 {
	    double s = sqrt (q.discriminant);
	    *r1 = (-b + s) / (2 * a);
	    *r2 = (-b - s) / (2 * a);
	    return 2;
	 }
       else if (q.discriminant == 0)
	 {
	    *r1 = -b / (2 * a);
	    return 1;
	 }
	 else return 0;
     }
*/

struct quadratic_test
{
  gcc_jit_context *ctxt;

  /* "double" and "(double *)".  */
  gcc_jit_type *numeric_type;
  gcc_jit_type *numeric_type_ptr;

  /* The value (double)0.  */
  gcc_jit_rvalue *zero;

  gcc_jit_type *int_type;
  gcc_jit_type *void_type;

  /* "struct quadratic" */
  gcc_jit_type *quadratic;
  gcc_jit_field *a;
  gcc_jit_field *b;
  gcc_jit_field *c;
  gcc_jit_field *discriminant;

  /* "(struct quadratic *)" */
  gcc_jit_type *quadratic_ptr;

  gcc_jit_function *calc_discriminant;

  gcc_jit_function *sqrt;

};

static void
make_types (struct quadratic_test *testcase)
{
  testcase->numeric_type =
    gcc_jit_context_get_type (testcase->ctxt, GCC_JIT_TYPE_DOUBLE);
  testcase->numeric_type_ptr =
    gcc_jit_type_get_pointer (testcase->numeric_type);
  testcase->zero =
    gcc_jit_context_zero (testcase->ctxt, testcase->numeric_type);

  testcase->int_type =
    gcc_jit_context_get_type (testcase->ctxt, GCC_JIT_TYPE_INT);

  testcase->void_type =
    gcc_jit_context_get_type (testcase->ctxt, GCC_JIT_TYPE_VOID);

  testcase->a =
    gcc_jit_context_new_field (testcase->ctxt,
			       NULL,
			       testcase->numeric_type,
			       "a");
  testcase->b =
    gcc_jit_context_new_field (testcase->ctxt,
			       NULL,
			       testcase->numeric_type,
			       "b");
  testcase->c =
    gcc_jit_context_new_field (testcase->ctxt,
			       NULL,
			       testcase->numeric_type,
			       "c");
  testcase->discriminant =
    gcc_jit_context_new_field (testcase->ctxt,
			       NULL,
			       testcase->numeric_type,
			       "discriminant");
  gcc_jit_field *fields[] = {testcase->a,
			     testcase->b,
			     testcase->c,
			     testcase->discriminant};
  testcase->quadratic =
    gcc_jit_struct_as_type (
      gcc_jit_context_new_struct_type (testcase->ctxt, NULL,
				       "quadratic", 4, fields));
  testcase->quadratic_ptr = gcc_jit_type_get_pointer (testcase->quadratic);
}

static void
make_sqrt (struct quadratic_test *testcase)
{
  gcc_jit_param *param_x =
    gcc_jit_context_new_param (testcase->ctxt, NULL,
			       testcase->numeric_type, "x");
  testcase->sqrt =
    gcc_jit_context_new_function (testcase->ctxt, NULL,
				  GCC_JIT_FUNCTION_IMPORTED,
				  testcase->numeric_type,
				  "sqrt",
				  1, &param_x,
				  0);
}

static void
make_calc_discriminant (struct quadratic_test *testcase)
{
  /* Build "calc_discriminant".  */
  gcc_jit_param *param_q =
    gcc_jit_context_new_param (testcase->ctxt, NULL,
			       testcase->quadratic_ptr, "q");
  testcase->calc_discriminant =
    gcc_jit_context_new_function (testcase->ctxt, NULL,
				  GCC_JIT_FUNCTION_INTERNAL,
				  testcase->void_type,
				  "calc_discriminant",
				  1, &param_q,
				  0);
  gcc_jit_block *blk =
    gcc_jit_function_new_block (testcase->calc_discriminant, NULL);
  gcc_jit_block_add_comment (
    blk, NULL,
    "(b^2 - 4ac)");

  gcc_jit_rvalue *q_a =
    gcc_jit_lvalue_as_rvalue (
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_param_as_rvalue (param_q),
	  NULL, testcase->a));
  gcc_jit_rvalue *q_b =
    gcc_jit_lvalue_as_rvalue (
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_param_as_rvalue (param_q),
	  NULL, testcase->b));
  gcc_jit_rvalue *q_c =
    gcc_jit_lvalue_as_rvalue (
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_param_as_rvalue (param_q),
	  NULL, testcase->c));

  /* (q->b * q->b) - (4 * q->a * q->c) */
  gcc_jit_rvalue *rhs =
    gcc_jit_context_new_binary_op (
      testcase->ctxt, NULL,
      GCC_JIT_BINARY_OP_MINUS,
      testcase->numeric_type,

      /* (q->b * q->b) */
      gcc_jit_context_new_binary_op (
	testcase->ctxt, NULL,
	GCC_JIT_BINARY_OP_MULT,
	testcase->numeric_type,
	q_b, q_b),

      /* (4 * (q->a * q->c)) */
      gcc_jit_context_new_binary_op (
	testcase->ctxt, NULL,
	GCC_JIT_BINARY_OP_MULT,
	testcase->numeric_type,
	/* 4.0 */
	gcc_jit_context_new_rvalue_from_int (
	  testcase->ctxt,
	  testcase->numeric_type,
	  4),
	/* (q->a * q->c) */
	gcc_jit_context_new_binary_op (
	  testcase->ctxt, NULL,
	  GCC_JIT_BINARY_OP_MULT,
	  testcase->numeric_type,
	  q_a, q_c)));

  CHECK_STRING_VALUE (
     gcc_jit_object_get_debug_string (gcc_jit_rvalue_as_object (rhs)),
     "q->b * q->b - (double)4 * q->a * q->c");

  gcc_jit_block_add_assignment (
    blk, NULL,

    /* q->discriminant =...  */
    gcc_jit_rvalue_dereference_field (
      gcc_jit_param_as_rvalue (param_q),
      NULL,
      testcase->discriminant),
    rhs);

  gcc_jit_block_end_with_void_return (blk, NULL);
}

static void
make_test_quadratic (struct quadratic_test *testcase)
{
  gcc_jit_param *a =
    gcc_jit_context_new_param (testcase->ctxt, NULL,
			       testcase->numeric_type, "a");
  gcc_jit_param *b =
    gcc_jit_context_new_param (testcase->ctxt, NULL,
			       testcase->numeric_type, "b");
  gcc_jit_param *c =
    gcc_jit_context_new_param (testcase->ctxt, NULL,
			       testcase->numeric_type, "c");
  gcc_jit_param *r1 =
    gcc_jit_context_new_param (testcase->ctxt, NULL,
			       testcase->numeric_type_ptr, "r1");
  gcc_jit_param *r2 =
    gcc_jit_context_new_param (testcase->ctxt, NULL,
			       testcase->numeric_type_ptr, "r2");
  gcc_jit_param *params[] = {a, b, c, r1, r2};
  gcc_jit_function *test_quadratic =
    gcc_jit_context_new_function (testcase->ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  testcase->int_type,
				  "test_quadratic",
				  5, params,
				  0);
  /* struct quadratic q; */
  gcc_jit_lvalue *q =
    gcc_jit_function_new_local (
      test_quadratic, NULL,
      testcase->quadratic,
      "q");

  gcc_jit_block *initial =
    gcc_jit_function_new_block (test_quadratic,
				"initial");
  gcc_jit_block *on_positive_discriminant
    = gcc_jit_function_new_block (test_quadratic,
				  "positive_discriminant");

  gcc_jit_block *on_nonpositive_discriminant
    = gcc_jit_function_new_block (test_quadratic,
				  "nonpositive_discriminant");

  gcc_jit_block *on_zero_discriminant
    = gcc_jit_function_new_block (test_quadratic,
				  "zero_discriminant");

  gcc_jit_block *on_negative_discriminant
    = gcc_jit_function_new_block (test_quadratic,
				  "negative_discriminant");

  /* Initial block.  */
  /* q.a = a; */
  gcc_jit_block_add_assignment (
    initial, NULL,
    gcc_jit_lvalue_access_field (q, NULL, testcase->a),
    gcc_jit_param_as_rvalue (a));
  /* q.b = b; */
  gcc_jit_block_add_assignment (
    initial, NULL,
    gcc_jit_lvalue_access_field (q, NULL, testcase->b),
    gcc_jit_param_as_rvalue (b));
  /* q.c = c; */
  gcc_jit_block_add_assignment (
    initial, NULL,
    gcc_jit_lvalue_access_field (q, NULL, testcase->c),
    gcc_jit_param_as_rvalue (c));
  /* calc_discriminant (&q); */
  gcc_jit_rvalue *address_of_q = gcc_jit_lvalue_get_address (q, NULL);
  gcc_jit_block_add_eval (
    initial, NULL,
    gcc_jit_context_new_call (
      testcase->ctxt, NULL,
      testcase->calc_discriminant,
      1, &address_of_q));

  gcc_jit_block_add_comment (
    initial, NULL,
    "if (q.discriminant > 0)");
  gcc_jit_block_end_with_conditional (
    initial, NULL,
    gcc_jit_context_new_comparison (
      testcase->ctxt, NULL,
      GCC_JIT_COMPARISON_GT,
      gcc_jit_rvalue_access_field (
	gcc_jit_lvalue_as_rvalue (q),
	NULL,
	testcase->discriminant),
      testcase->zero),
    on_positive_discriminant,
    on_nonpositive_discriminant);

  /* Block: "on_positive_discriminant" */
  /* double s = sqrt (q.discriminant); */
  gcc_jit_lvalue *s = gcc_jit_function_new_local (
    test_quadratic, NULL,
    testcase->numeric_type,
    "s");
  gcc_jit_rvalue *discriminant_of_q =
    gcc_jit_rvalue_access_field (gcc_jit_lvalue_as_rvalue (q),
				 NULL,
				 testcase->discriminant);
  gcc_jit_block_add_assignment (
    on_positive_discriminant, NULL,
    s,
    gcc_jit_context_new_call (
      testcase->ctxt, NULL,
      testcase->sqrt,
      1, &discriminant_of_q));

  gcc_jit_rvalue *minus_b =
    gcc_jit_context_new_unary_op (
      testcase->ctxt, NULL,
      GCC_JIT_UNARY_OP_MINUS,
      testcase->numeric_type,
      gcc_jit_param_as_rvalue (b));
  gcc_jit_rvalue *two_a =
    gcc_jit_context_new_binary_op (
      testcase->ctxt, NULL,
      GCC_JIT_BINARY_OP_MULT,
      testcase->numeric_type,
      gcc_jit_context_new_rvalue_from_int (
	testcase->ctxt,
	testcase->numeric_type,
	2),
      gcc_jit_param_as_rvalue (a));

  gcc_jit_block_add_comment (
    on_positive_discriminant, NULL,
    "*r1 = (-b + s) / (2 * a);");
  gcc_jit_block_add_assignment (
    on_positive_discriminant, NULL,

    /* "*r1 = ..." */
    gcc_jit_rvalue_dereference (
      gcc_jit_param_as_rvalue (r1), NULL),

    /* (-b + s) / (2 * a) */
    gcc_jit_context_new_binary_op (
      testcase->ctxt, NULL,
      GCC_JIT_BINARY_OP_DIVIDE,
      testcase->numeric_type,
      gcc_jit_context_new_binary_op (
	testcase->ctxt, NULL,
	GCC_JIT_BINARY_OP_PLUS,
	testcase->numeric_type,
	minus_b,
	gcc_jit_lvalue_as_rvalue (s)),
      two_a));

  gcc_jit_block_add_comment (
    on_positive_discriminant, NULL,
    "*r2 = (-b - s) / (2 * a)");
  gcc_jit_block_add_assignment (
    on_positive_discriminant, NULL,

    /* "*r2 = ..." */
    gcc_jit_rvalue_dereference (
      gcc_jit_param_as_rvalue (r2), NULL),

    /* (-b - s) / (2 * a) */
    gcc_jit_context_new_binary_op (
      testcase->ctxt, NULL,
      GCC_JIT_BINARY_OP_DIVIDE,
      testcase->numeric_type,
      gcc_jit_context_new_binary_op (
	testcase->ctxt, NULL,
	GCC_JIT_BINARY_OP_MINUS,
	testcase->numeric_type,
	minus_b,
	gcc_jit_lvalue_as_rvalue (s)),
      two_a));

  /* "return 2;" */
  gcc_jit_block_end_with_return (
    on_positive_discriminant, NULL,
    gcc_jit_context_new_rvalue_from_int (
      testcase->ctxt,
      testcase->int_type,
      2));

  /* Block: "on_nonpositive_discriminant" */
  gcc_jit_block_add_comment (
    on_nonpositive_discriminant, NULL,
    "else if (q.discriminant == 0)");
  gcc_jit_block_end_with_conditional (
    on_nonpositive_discriminant, NULL,
    gcc_jit_context_new_comparison (
      testcase->ctxt, NULL,
      GCC_JIT_COMPARISON_EQ,
      gcc_jit_rvalue_access_field (
	gcc_jit_lvalue_as_rvalue (q),
	NULL,
	testcase->discriminant),
      testcase->zero),
    on_zero_discriminant,
    on_negative_discriminant);

  /* Block: "on_zero_discriminant" */
  gcc_jit_block_add_comment (
    on_zero_discriminant, NULL,
    "*r1 = -b / (2 * a);");
  gcc_jit_block_add_assignment (
    on_zero_discriminant, NULL,

    /* "*r1 = ..." */
    gcc_jit_rvalue_dereference (
      gcc_jit_param_as_rvalue (r1), NULL),

    /* -b / (2 * a) */
    gcc_jit_context_new_binary_op (
      testcase->ctxt, NULL,
      GCC_JIT_BINARY_OP_DIVIDE,
      testcase->numeric_type,
      minus_b,
      two_a));
  gcc_jit_block_end_with_return (
    /* "return 1;" */
    on_zero_discriminant, NULL,
      gcc_jit_context_one (testcase->ctxt, testcase->int_type));

  /* Block: "on_negative_discriminant" */
  gcc_jit_block_end_with_return (
    /* "else return 0;" */
    on_negative_discriminant, NULL,
    gcc_jit_context_zero (testcase->ctxt, testcase->int_type));
}

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  struct quadratic_test testcase;
  memset (&testcase, 0, sizeof (testcase));
  testcase.ctxt = ctxt;
  make_types (&testcase);
  make_sqrt (&testcase);
  make_calc_discriminant (&testcase);
  make_test_quadratic (&testcase);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*fn_type) (double a, double b, double c,
			  double *r1, double *r2);

  CHECK_NON_NULL (result);

  fn_type test_quadratic =
    (fn_type)gcc_jit_result_get_code (result, "test_quadratic");
  CHECK_NON_NULL (test_quadratic);

  /* Verify that the code correctly solves quadratic equations.  */
  double r1, r2;

  /* This one has two solutions: */
  CHECK_VALUE (test_quadratic (1, 3, -4, &r1, &r2), 2);
  CHECK_VALUE (r1, 1);
  CHECK_VALUE (r2, -4);

  /* This one has one solution: */
  CHECK_VALUE (test_quadratic (4, 4, 1, &r1, &r2), 1);
  CHECK_VALUE (r1, -0.5);

  /* This one has no real solutions: */
  CHECK_VALUE (test_quadratic (4, 1, 1, &r1, &r2), 0);
}
