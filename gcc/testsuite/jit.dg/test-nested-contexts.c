#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#define TEST_ESCHEWS_TEST_JIT
#define TEST_PROVIDES_MAIN
#include "harness.h"

struct quadratic
{
  double a;
  double b;
  double c;
  double discriminant;
};

/* This is an adapted version of test-quadratic.c

   Like that test, we'll try to inject the following code, but we'll
   split it up into some nested contexts, in 3 levels, to test
   how nested contexts work.

   ***** In top-level context: *****

     (shared type declarations, for int, double, struct quadratic);
     extern double sqrt (double);

   ***** In mid-level context: *****

     void
     calc_discriminant (struct quadratic *q)
     {
       // (b^2 - 4ac)
       q->discriminant = (q->b * q->b) - (4 * q->a * q->c);
     }

   ***** In bottom context: *****

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

struct top_level
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
  gcc_jit_type *struct_quadratic;
  gcc_jit_field *a;
  gcc_jit_field *b;
  gcc_jit_field *c;
  gcc_jit_field *discriminant;

  /* "(struct quadratic *)" */
  gcc_jit_type *quadratic_ptr;

  gcc_jit_function *sqrt;
};

struct middle_level
{
  gcc_jit_context *ctxt;
  gcc_jit_function *calc_discriminant;
};

struct bottom_level
{
  gcc_jit_context *ctxt;
};

static void
make_types (struct top_level *top_level)
{
  top_level->numeric_type =
    gcc_jit_context_get_type (top_level->ctxt, GCC_JIT_TYPE_DOUBLE);
  top_level->numeric_type_ptr =
    gcc_jit_type_get_pointer (top_level->numeric_type);
  top_level->zero =
    gcc_jit_context_zero (top_level->ctxt, top_level->numeric_type);

  top_level->int_type =
    gcc_jit_context_get_type (top_level->ctxt, GCC_JIT_TYPE_INT);
  top_level->void_type =
    gcc_jit_context_get_type (top_level->ctxt, GCC_JIT_TYPE_VOID);

  top_level->a =
    gcc_jit_context_new_field (top_level->ctxt,
			       NULL,
			       top_level->numeric_type,
			       "a");
  top_level->b =
    gcc_jit_context_new_field (top_level->ctxt,
			       NULL,
			       top_level->numeric_type,
			       "b");
  top_level->c =
    gcc_jit_context_new_field (top_level->ctxt,
			       NULL,
			       top_level->numeric_type,
			       "c");
  top_level->discriminant =
    gcc_jit_context_new_field (top_level->ctxt,
			       NULL,
			       top_level->numeric_type,
			       "discriminant");
  gcc_jit_field *fields[] = {top_level->a,
			     top_level->b,
			     top_level->c,
			     top_level->discriminant};
  top_level->struct_quadratic =
    gcc_jit_struct_as_type (
      gcc_jit_context_new_struct_type (top_level->ctxt, NULL,
				       "quadratic", 4, fields));
  top_level->quadratic_ptr =
    gcc_jit_type_get_pointer (top_level->struct_quadratic);
}

static void
make_sqrt (struct top_level *top_level)
{
  gcc_jit_param *param_x =
    gcc_jit_context_new_param (top_level->ctxt, NULL,
			       top_level->numeric_type, "x");
  top_level->sqrt =
    gcc_jit_context_new_function (top_level->ctxt, NULL,
				  GCC_JIT_FUNCTION_IMPORTED,
				  top_level->numeric_type,
				  "sqrt",
				  1, &param_x,
				  0);
}

static void
make_calc_discriminant (struct top_level *top_level,
			struct middle_level *middle_level)
{
  /* Build "calc_discriminant".  */
  gcc_jit_param *param_q =
    gcc_jit_context_new_param (middle_level->ctxt, NULL,
			       top_level->quadratic_ptr, "q");
  middle_level->calc_discriminant =
    gcc_jit_context_new_function (middle_level->ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  top_level->void_type,
				  "calc_discriminant",
				  1, &param_q,
				  0);
  gcc_jit_block *blk =
    gcc_jit_function_new_block (middle_level->calc_discriminant, NULL);
  gcc_jit_block_add_comment (
    blk, NULL,
    "(b^2 - 4ac)");

  gcc_jit_rvalue *q_a =
    gcc_jit_lvalue_as_rvalue (
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_param_as_rvalue (param_q),
	  NULL, top_level->a));
  gcc_jit_rvalue *q_b =
    gcc_jit_lvalue_as_rvalue (
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_param_as_rvalue (param_q),
	  NULL, top_level->b));
  gcc_jit_rvalue *q_c =
    gcc_jit_lvalue_as_rvalue (
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_param_as_rvalue (param_q),
	  NULL, top_level->c));

  gcc_jit_block_add_assignment (
    blk, NULL,

    /* q->discriminant =...  */
    gcc_jit_rvalue_dereference_field (
      gcc_jit_param_as_rvalue (param_q),
      NULL,
      top_level->discriminant),

    /* (q->b * q->b) - (4 * q->a * q->c) */
    gcc_jit_context_new_binary_op (
      middle_level->ctxt, NULL,
      GCC_JIT_BINARY_OP_MINUS,
      top_level->numeric_type,

      /* (q->b * q->b) */
      gcc_jit_context_new_binary_op (
	middle_level->ctxt, NULL,
	GCC_JIT_BINARY_OP_MULT,
	top_level->numeric_type,
	q_b, q_b),

      /* (4 * (q->a * q->c)) */
      gcc_jit_context_new_binary_op (
	middle_level->ctxt, NULL,
	GCC_JIT_BINARY_OP_MULT,
	top_level->numeric_type,
	/* 4.0 */
	gcc_jit_context_new_rvalue_from_int (
	  middle_level->ctxt,
	  top_level->numeric_type,
	  4),
	/* (q->a * q->c) */
	gcc_jit_context_new_binary_op (
	  middle_level->ctxt, NULL,
	  GCC_JIT_BINARY_OP_MULT,
	  top_level->numeric_type,
	  q_a, q_c)))); /* end of gcc_jit_function_add_assignment call.  */

  gcc_jit_block_end_with_void_return (blk, NULL);
}

static void
make_test_quadratic (struct top_level *top_level,
		     struct middle_level *middle_level,
		     struct bottom_level *bottom_level)
{
  gcc_jit_param *a =
    gcc_jit_context_new_param (bottom_level->ctxt, NULL,
			       top_level->numeric_type, "a");
  gcc_jit_param *b =
    gcc_jit_context_new_param (bottom_level->ctxt, NULL,
			       top_level->numeric_type, "b");
  gcc_jit_param *c =
    gcc_jit_context_new_param (bottom_level->ctxt, NULL,
			       top_level->numeric_type, "c");
  gcc_jit_param *r1 =
    gcc_jit_context_new_param (bottom_level->ctxt, NULL,
			       top_level->numeric_type_ptr, "r1");
  gcc_jit_param *r2 =
    gcc_jit_context_new_param (bottom_level->ctxt, NULL,
			       top_level->numeric_type_ptr, "r2");
  gcc_jit_param *params[] = {a, b, c, r1, r2};
  gcc_jit_function *test_quadratic =
    gcc_jit_context_new_function (bottom_level->ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  top_level->int_type,
				  "test_quadratic",
				  5, params,
				  0);

  /* struct quadratic q; */
  gcc_jit_lvalue *q =
    gcc_jit_function_new_local (
      test_quadratic, NULL,
      top_level->struct_quadratic,
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
    gcc_jit_lvalue_access_field (q, NULL, top_level->a),
    gcc_jit_param_as_rvalue (a));
  /* q.b = b; */
  gcc_jit_block_add_assignment (
    initial, NULL,
    gcc_jit_lvalue_access_field (q, NULL, top_level->b),
    gcc_jit_param_as_rvalue (b));
  /* q.c = c; */
  gcc_jit_block_add_assignment (
    initial, NULL,
    gcc_jit_lvalue_access_field (q, NULL, top_level->c),
    gcc_jit_param_as_rvalue (c));
  /* calc_discriminant (&q); */
  gcc_jit_rvalue *address_of_q = gcc_jit_lvalue_get_address (q, NULL);
  gcc_jit_block_add_eval (
    initial, NULL,
    gcc_jit_context_new_call (
      bottom_level->ctxt, NULL,
      middle_level->calc_discriminant,
      1, &address_of_q));

  gcc_jit_block_add_comment (
    initial, NULL,
    "if (q.discriminant > 0)");
  gcc_jit_block_end_with_conditional (
    initial, NULL,
    gcc_jit_context_new_comparison (
      bottom_level->ctxt, NULL,
      GCC_JIT_COMPARISON_GT,
      gcc_jit_rvalue_access_field (
	gcc_jit_lvalue_as_rvalue (q),
	NULL,
	top_level->discriminant),
      top_level->zero),
    on_positive_discriminant,
    on_nonpositive_discriminant);

  /* Block: "on_positive_discriminant" */
  /* double s = sqrt (q.discriminant); */
  gcc_jit_lvalue *s = gcc_jit_function_new_local (
    test_quadratic, NULL,
    top_level->numeric_type,
    "s");
  gcc_jit_rvalue *discriminant_of_q =
    gcc_jit_rvalue_access_field (gcc_jit_lvalue_as_rvalue (q),
				 NULL,
				 top_level->discriminant);
  gcc_jit_block_add_assignment (
    on_positive_discriminant, NULL,
    s,
    gcc_jit_context_new_call (
      bottom_level->ctxt, NULL,
      top_level->sqrt,
      1, &discriminant_of_q));

  gcc_jit_rvalue *minus_b =
    gcc_jit_context_new_unary_op (
      bottom_level->ctxt, NULL,
      GCC_JIT_UNARY_OP_MINUS,
      top_level->numeric_type,
      gcc_jit_param_as_rvalue (b));
  gcc_jit_rvalue *two_a =
    gcc_jit_context_new_binary_op (
      bottom_level->ctxt, NULL,
      GCC_JIT_BINARY_OP_MULT,
      top_level->numeric_type,
      gcc_jit_context_new_rvalue_from_int (
	bottom_level->ctxt,
	top_level->numeric_type,
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
      bottom_level->ctxt, NULL,
      GCC_JIT_BINARY_OP_DIVIDE,
      top_level->numeric_type,
      gcc_jit_context_new_binary_op (
	bottom_level->ctxt, NULL,
	GCC_JIT_BINARY_OP_PLUS,
	top_level->numeric_type,
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
      bottom_level->ctxt, NULL,
      GCC_JIT_BINARY_OP_DIVIDE,
      top_level->numeric_type,
      gcc_jit_context_new_binary_op (
	bottom_level->ctxt, NULL,
	GCC_JIT_BINARY_OP_MINUS,
	top_level->numeric_type,
	minus_b,
	gcc_jit_lvalue_as_rvalue (s)),
      two_a));

  /* "return 2;" */
  gcc_jit_block_end_with_return (
    on_positive_discriminant, NULL,
    gcc_jit_context_new_rvalue_from_int (
      bottom_level->ctxt,
      top_level->int_type,
      2));

  /* Block: "on_nonpositive_discriminant" */
  gcc_jit_block_add_comment (
    on_nonpositive_discriminant, NULL,
    "else if (q.discriminant == 0)");
  gcc_jit_block_end_with_conditional (
    on_nonpositive_discriminant, NULL,
    gcc_jit_context_new_comparison (
      bottom_level->ctxt, NULL,
      GCC_JIT_COMPARISON_EQ,
      gcc_jit_rvalue_access_field (
	gcc_jit_lvalue_as_rvalue (q),
	NULL,
	top_level->discriminant),
      top_level->zero),
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
      bottom_level->ctxt, NULL,
      GCC_JIT_BINARY_OP_DIVIDE,
      top_level->numeric_type,
      minus_b,
      two_a));

  /* "return 1;" */
  gcc_jit_block_end_with_return (
    on_zero_discriminant, NULL,
      gcc_jit_context_one (bottom_level->ctxt, top_level->int_type));

  /* Block: "on_negative_discriminant" */
  gcc_jit_block_end_with_return (
    /* else return 0; */
    on_negative_discriminant, NULL,
    gcc_jit_context_zero (bottom_level->ctxt, top_level->int_type));
}

void
verify_middle_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  struct quadratic q;

  typedef void (*fn_type) (struct quadratic *q);
  fn_type calc_discriminant =
    (fn_type)gcc_jit_result_get_code (result,
				      "calc_discriminant");
  CHECK_NON_NULL (calc_discriminant);

  q.a = 3;
  q.b = 5;
  q.c = 7;
  q.discriminant = 0;
  calc_discriminant (&q);

  CHECK_VALUE (q.discriminant, -59);
}

void
verify_bottom_code (gcc_jit_context *ctxt, gcc_jit_result *result)
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

int
main (int argc, char **argv)
{
  int i, j, k;
  const int NUM_TOP_ITERATIONS = 2;
  const int NUM_MIDDLE_ITERATIONS = 2;
  const int NUM_BOTTOM_ITERATIONS = 2;

  /* We do the whole thing multiple times to shake out state-management
     issues in the underlying code.  */

  FILE *logfile = fopen ("test-nested-contexts.c.exe.log.txt", "w");
  if (!logfile)
    fail ("error opening logfile");

  for (i = 1; i <= NUM_TOP_ITERATIONS; i++)
    {
      /* Create the top-level context.  */
      snprintf (test, sizeof (test),
		"%s iteration %d of %d of top level",
		extract_progname (argv[0]),
		i, NUM_TOP_ITERATIONS);

      struct top_level top_level;
      memset (&top_level, 0, sizeof (top_level));

      top_level.ctxt = gcc_jit_context_acquire ();
      gcc_jit_context_set_logfile (top_level.ctxt,
				   logfile,
				   0, 0);
      set_options (top_level.ctxt, argv[0]);

      make_types (&top_level);
      make_sqrt (&top_level);

      /* No errors should have occurred.  */
      CHECK_VALUE (gcc_jit_context_get_first_error (top_level.ctxt), NULL);

      gcc_jit_context_dump_to_file (top_level.ctxt,
				    "dump-of-test-nested-contexts-top.c",
				    1);

      for (j = 1; j <= NUM_MIDDLE_ITERATIONS; j++)
	{
	  /* Create and populate the middle-level context, using
	     objects from the top-level context.  */
	  snprintf (test, sizeof (test),
		    ("%s iteration %d of %d of top level;"
		     " %d of %d of middle level"),
		    extract_progname (argv[0]),
		    i, NUM_TOP_ITERATIONS,
		    j, NUM_MIDDLE_ITERATIONS);

	  struct middle_level middle_level;
	  memset (&middle_level, 0, sizeof (middle_level));

	  middle_level.ctxt =
	    gcc_jit_context_new_child_context (top_level.ctxt);
	  make_calc_discriminant (&top_level,
				  &middle_level);

	  /* No errors should have occurred.  */
	  CHECK_VALUE (gcc_jit_context_get_first_error (middle_level.ctxt),
		       NULL);

	  gcc_jit_context_dump_to_file (middle_level.ctxt,
					"dump-of-test-nested-contexts-middle.c",
					1);

	  gcc_jit_result *middle_result =
	    gcc_jit_context_compile (middle_level.ctxt);
	  CHECK_NON_NULL (middle_result);

	  verify_middle_code (middle_level.ctxt, middle_result);

	  for (k = 1; k <= NUM_BOTTOM_ITERATIONS; k++)
	    {
	      /* Create and populate the innermost context, using
		 objects from the top-level and middle-level contexts.  */
	      snprintf (test, sizeof (test),
			("%s iteration %d of %d of top level;"
			 " %d of %d of middle level;"
			 " %d of %d of bottom level"),
			extract_progname (argv[0]),
			i, NUM_TOP_ITERATIONS,
			j, NUM_MIDDLE_ITERATIONS,
			k, NUM_BOTTOM_ITERATIONS);

	      struct bottom_level bottom_level;
	      memset (&bottom_level, 0, sizeof (bottom_level));

	      bottom_level.ctxt =
		gcc_jit_context_new_child_context (middle_level.ctxt);
	      make_test_quadratic (&top_level,
				   &middle_level,
				   &bottom_level);

	      /* No errors should have occurred.  */
	      CHECK_VALUE (gcc_jit_context_get_first_error (bottom_level.ctxt),
			   NULL);

	      gcc_jit_context_dump_to_file (bottom_level.ctxt,
					    "dump-of-test-nested-contexts-bottom.c",
					    1);

	      /* Dump a reproducer for the bottom context.
		 The generated reproducer needs to also regenerate the
		 parent contexts, so this gives us test coverage for
		 that case.  */
	      gcc_jit_context_dump_reproducer_to_file (
		bottom_level.ctxt,
		"test-nested-contexts.c.exe.reproducer.c");

	      gcc_jit_result *bottom_result =
		gcc_jit_context_compile (bottom_level.ctxt);
	      verify_bottom_code (bottom_level.ctxt, bottom_result);
	      gcc_jit_result_release (bottom_result);
	      gcc_jit_context_release (bottom_level.ctxt);

	    }

	  gcc_jit_result_release (middle_result);
	  gcc_jit_context_release (middle_level.ctxt);

	}

      gcc_jit_context_release (top_level.ctxt);
   }

  if (logfile)
    fclose (logfile);

  totals ();

  return 0;
}
