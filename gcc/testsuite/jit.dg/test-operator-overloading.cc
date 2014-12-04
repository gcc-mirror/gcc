/* Test of C++ API.  */
#include <stdlib.h>
#include <stdio.h>

#include "libgccjit++.h"

#include <sstream>

#include "harness.h"

struct quadratic
{
  double a;
  double b;
  double c;
  double discriminant;
};

/* As per test-quadratic.cc, let's try to inject the equivalent of:

     extern double sqrt (double);

     void
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

  However, we'll use operator overloading for maxium brevity, at the
  risk of perhaps being too "magical".
*/

/****************************************************************************
 Test case
 ****************************************************************************/

struct quadratic_test
{
  gccjit::context ctxt;

  /* "double" and "(double *)".  */
  gccjit::type numeric_type;
  gccjit::type numeric_type_ptr;

  /* The value (double)0.  */
  gccjit::rvalue zero;

  gccjit::type int_type;
  gccjit::type void_type;

  /* "struct quadratic" */
  gccjit::type quadratic;
  gccjit::field a;
  gccjit::field b;
  gccjit::field c;
  gccjit::field discriminant;

  /* "(struct quadratic *)" */
  gccjit::type quadratic_ptr;

  gccjit::function calc_discriminant;

  gccjit::function sqrt;

};

static void
make_types (quadratic_test &testcase)
{
  testcase.numeric_type = testcase.ctxt.get_type (GCC_JIT_TYPE_DOUBLE);
  testcase.numeric_type_ptr = testcase.numeric_type.get_pointer ();
  testcase.zero = testcase.ctxt.zero (testcase.numeric_type);

  testcase.int_type = testcase.ctxt.get_int_type <int> ();
  testcase.void_type = testcase.ctxt.get_type (GCC_JIT_TYPE_VOID);

  testcase.a = testcase.ctxt.new_field (testcase.numeric_type, "a");
  testcase.b = testcase.ctxt.new_field (testcase.numeric_type, "b");
  testcase.c = testcase.ctxt.new_field (testcase.numeric_type, "c");
  testcase.discriminant =
    testcase.ctxt.new_field (testcase.numeric_type, "discriminant");
  CHECK_STRING_VALUE (testcase.discriminant.get_debug_string ().c_str (),
                      "discriminant");
  std::vector<gccjit::field> fields (4);
  fields[0] = testcase.a;
  fields[1] = testcase.b;
  fields[2] = testcase.c;
  fields[3] = testcase.discriminant;
  testcase.quadratic =
    testcase.ctxt.new_struct_type (
      "quadratic",
      fields);
  testcase.quadratic_ptr = testcase.quadratic.get_pointer ();
}

static void
make_sqrt (quadratic_test &testcase)
{
  std::vector<gccjit::param> params (1);
  params[0] =
    testcase.ctxt.new_param (testcase.numeric_type, "x");
  testcase.sqrt =
    testcase.ctxt.new_function (GCC_JIT_FUNCTION_IMPORTED,
				testcase.numeric_type,
				"sqrt",
				params,
				0);
}

static void
make_calc_discriminant (quadratic_test &testcase)
{
  /* Build "calc_discriminant".  */
  gccjit::param param_q =
    testcase.ctxt.new_param (testcase.quadratic_ptr, "q");
  std::vector <gccjit::param> params (1);
  params[0] = param_q;
  testcase.calc_discriminant =
    testcase.ctxt.new_function (GCC_JIT_FUNCTION_EXPORTED,
				testcase.void_type,
				"calc_discriminant",
				params,
				0);
  gccjit::block block = testcase.calc_discriminant.new_block ();
  block.add_comment ("(b^2 - 4ac)");

  gccjit::rvalue q_a = param_q.dereference_field (testcase.a);
  gccjit::rvalue q_b = param_q.dereference_field (testcase.b);
  gccjit::rvalue q_c = param_q.dereference_field (testcase.c);

  gccjit::rvalue four =
    testcase.ctxt.new_rvalue (testcase.numeric_type, 4);

  block.add_assignment (
    /* q->discriminant =...  */
    param_q.dereference_field (testcase.discriminant),
    /* (q->b * q->b) - (4 * q->a * q->c) */
    (q_b * q_b) - (four * q_a * q_c));
  block.end_with_return ();
}

static void
make_test_quadratic (quadratic_test &testcase)
{
  gccjit::param a = testcase.ctxt.new_param (testcase.numeric_type, "a");
  gccjit::param b = testcase.ctxt.new_param (testcase.numeric_type, "b");
  gccjit::param c = testcase.ctxt.new_param (testcase.numeric_type, "c");
  gccjit::param r1 =
    testcase.ctxt.new_param (testcase.numeric_type_ptr, "r1");
  gccjit::param r2 =
    testcase.ctxt.new_param (testcase.numeric_type_ptr, "r2");

  std::vector<gccjit::param> params (5);
  params[0] = a;
  params[1] = b;
  params[2] = c;
  params[3] = r1;
  params[4] = r2;

  gccjit::function test_quadratic =
    testcase.ctxt.new_function (GCC_JIT_FUNCTION_EXPORTED,
				testcase.int_type,
				"test_quadratic",
				params,
				0);

  /* struct quadratic q; */
  gccjit::lvalue q = test_quadratic.new_local (testcase.quadratic, "q");

  gccjit::block initial = test_quadratic.new_block ("initial");
  gccjit::block on_positive_discriminant
    = test_quadratic.new_block ("positive_discriminant");
  gccjit::block on_nonpositive_discriminant
    = test_quadratic.new_block ("nonpositive_discriminant");
  gccjit::block on_zero_discriminant
    = test_quadratic.new_block ("zero_discriminant");
  gccjit::block on_negative_discriminant
    = test_quadratic.new_block ("negative_discriminant");

  CHECK_STRING_VALUE (on_zero_discriminant.get_debug_string ().c_str (),
                      "zero_discriminant");

  /* q.a = a; */
  initial.add_assignment (q.access_field (testcase.a), a);
  /* q.b = b; */
  initial.add_assignment (q.access_field (testcase.b), b);
  /* q.c = c; */
  initial.add_assignment (q.access_field (testcase.c), c);
  /* calc_discriminant (&q); */
  gccjit::rvalue address_of_q = q.get_address ();
  initial.add_eval (testcase.calc_discriminant (address_of_q));

  initial.add_comment ("if (q.discriminant > 0)");
  initial.end_with_conditional (
    q.access_field (testcase.discriminant) > testcase.zero,
    on_positive_discriminant,
    on_nonpositive_discriminant);

  /* Block: "on_positive_discriminant" */
  /* double s = sqrt (q.discriminant); */
  gccjit::lvalue s = test_quadratic.new_local (testcase.numeric_type, "s");
  gccjit::rvalue discriminant_of_q = q.access_field (testcase.discriminant);
  on_positive_discriminant.add_assignment (s, testcase.sqrt (discriminant_of_q));

  gccjit::rvalue minus_b = -b;
  gccjit::rvalue two =
    testcase.ctxt.new_rvalue (testcase.numeric_type, 2);
  gccjit::rvalue two_a = two * a;
  CHECK_STRING_VALUE (two_a.get_debug_string ().c_str (),
                      "(double)2 * a");

  on_positive_discriminant.add_comment ("*r1 = (-b + s) / (2 * a);");
  on_positive_discriminant.add_assignment (*r1, (minus_b + s) / two_a);

  on_positive_discriminant.add_comment ("*r2 = (-b - s) / (2 * a)");
  on_positive_discriminant.add_assignment (*r2, (minus_b - s) / two_a);

  /* "return 2;" */
  on_positive_discriminant.end_with_return (
    testcase.ctxt.new_rvalue (testcase.int_type, 2));

  /* Block: "on_nonpositive_discriminant" */
  /* "else if (q.discriminant == 0)" */
  on_nonpositive_discriminant.add_comment ("else if (q.discriminant == 0)");
  on_nonpositive_discriminant.end_with_conditional (
    q.access_field (testcase.discriminant) == testcase.zero,
    on_zero_discriminant,
    on_negative_discriminant);

  /* Block: "on_zero_discriminant" */
  /* if (q.discriminant == 0) */
  on_zero_discriminant.add_comment ("*r1 = -b / (2 * a);");
  on_zero_discriminant.add_assignment (*r1, minus_b / two_a);

  /* "return 1;" */
  on_zero_discriminant.end_with_return (testcase.int_type.one ());

  /* Block: "on_negative_discriminant" */
  /* else return 0; */
  on_negative_discriminant.end_with_return (testcase.int_type.zero ());

  /* Verify that output stream operator << works.  */
  std::ostringstream os;
  os << "streamed output: " << address_of_q;
  CHECK_STRING_VALUE (os.str ().c_str (), "streamed output: &q");
}

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  struct quadratic_test testcase;
  memset (&testcase, 0, sizeof (testcase));
  testcase.ctxt = ctxt;
  make_types (testcase);
  make_sqrt (testcase);
  make_calc_discriminant (testcase);
  make_test_quadratic (testcase);
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
