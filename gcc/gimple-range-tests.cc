/* Unit tests for GIMPLE range related routines.
   Copyright (C) 2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#if CHECKING_P

#include "selftest.h"

namespace selftest {

// Test ranges of tree expressions.
class test_expr_eval : public gimple_ranger
{
public:
  test_expr_eval ()
  {
    type = integer_type_node;
    op0 = make_ssa_name (type);
    op1 = make_ssa_name (type);

    // [5,10] + [15,20] => [20, 30]
    tree expr = fold_build2 (PLUS_EXPR, type, op0, op1);
    int_range<2> expect (build_int_cst (type, 20), build_int_cst (type, 30));
    int_range_max r;

    ASSERT_TRUE (range_of_expr (r, expr));
    ASSERT_TRUE (r == expect);
  }

  virtual bool range_of_expr (irange &r, tree expr, gimple * = NULL) OVERRIDE
  {
    if (expr == op0)
      {
	r.set (build_int_cst (type, 5), build_int_cst (type, 10));
	return true;
      }
    if (expr == op1)
      {
	r.set (build_int_cst (type, 15), build_int_cst (type, 20));
	return true;
      }
    return gimple_ranger::range_of_expr (r, expr);
  }

private:
  tree op0, op1, type;
};

void
gimple_range_tests ()
{
  test_expr_eval e;
}

} // namespace selftest

#endif // CHECKING_P
