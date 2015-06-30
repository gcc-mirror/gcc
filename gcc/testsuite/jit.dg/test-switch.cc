#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgccjit++.h"

#include "harness.h"

/* Quote from here in docs/cp/topics/functions.rst.  */

void
create_code (gcc_jit_context *c_ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
      int
      test_switch (int x)
      {
	switch (x)
	  {
	  case 0 ... 5:
	     return 3;

	  case 25 ... 27:
	     return 4;

	  case -42 ... -17:
	     return 83;

	  case 40:
	     return 8;

	  default:
	     return 10;
	  }
      }
   */
  gccjit::context ctxt (c_ctxt);
  gccjit::type t_int = ctxt.get_type (GCC_JIT_TYPE_INT);
  gccjit::type return_type = t_int;
  gccjit::param x = ctxt.new_param (t_int, "x");
  std::vector <gccjit::param> params;
  params.push_back (x);
  gccjit::function func = ctxt.new_function (GCC_JIT_FUNCTION_EXPORTED,
                                             return_type,
                                             "test_switch",
                                             params, 0);

  gccjit::block b_initial = func.new_block ("initial");

  gccjit::block b_default = func.new_block ("default");
  gccjit::block b_case_0_5 = func.new_block ("case_0_5");
  gccjit::block b_case_25_27 = func.new_block ("case_25_27");
  gccjit::block b_case_m42_m17 = func.new_block ("case_m42_m17");
  gccjit::block b_case_40 = func.new_block ("case_40");

  std::vector <gccjit::case_> cases;
  cases.push_back (ctxt.new_case (ctxt.new_rvalue (t_int, 0),
                                  ctxt.new_rvalue (t_int, 5),
                                  b_case_0_5));
  cases.push_back (ctxt.new_case (ctxt.new_rvalue (t_int, 25),
                                  ctxt.new_rvalue (t_int, 27),
                                  b_case_25_27));
  cases.push_back (ctxt.new_case (ctxt.new_rvalue (t_int, -42),
                                  ctxt.new_rvalue (t_int, -17),
                                  b_case_m42_m17));
  cases.push_back (ctxt.new_case (ctxt.new_rvalue (t_int, 40),
                                  ctxt.new_rvalue (t_int, 40),
                                  b_case_40));
  b_initial.end_with_switch (x,
                             b_default,
                             cases);

  b_case_0_5.end_with_return (ctxt.new_rvalue (t_int, 3));
  b_case_25_27.end_with_return (ctxt.new_rvalue (t_int, 4));
  b_case_m42_m17.end_with_return (ctxt.new_rvalue (t_int, 83));
  b_case_40.end_with_return (ctxt.new_rvalue (t_int, 8));
  b_default.end_with_return (ctxt.new_rvalue (t_int, 10));
}

/* Quote up to here in docs/cp/topics/functions.rst.  */

static int
c_test_switch (int x)
{
  switch (x)
    {
    case 0 ... 5:
      return 3;
    case 25 ... 27:
      return 4;
    case -42 ... -17:
      return 83;
    case 40:
      return 8;
    default:
      return 10;
    }
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*test_switch_type) (int);
  CHECK_NON_NULL (result);
  test_switch_type test_switch =
    (test_switch_type)gcc_jit_result_get_code (result, "test_switch");
  CHECK_NON_NULL (test_switch);

  int i;

  for (i = -255; i < 255; i++)
    {
      int val = test_switch (i);
      int exp = c_test_switch (i);
      if (val != exp)
	fail ("test_switch (%i) returned: %i; expected; %i", i, val, exp);
    }
}
