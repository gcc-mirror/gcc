/* Lower and optimize address expressions.
   Copyright (C) 2015-2021 Free Software Foundation, Inc.
   Contributed by Marek Polacek <polacek@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "alias.h"
#include "predict.h"
#include "tm.h"
#include "function.h"
#include "dominance.h"
#include "cfg.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "symtab.h"
#include "tree.h"
#include "stringpool.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "fold-const.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-pass.h"


namespace {

const pass_data pass_data_laddress =
{
  GIMPLE_PASS, /* type */
  "laddress", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_GIMPLE_LADDRESS, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_laddress : public gimple_opt_pass
{
public:
  pass_laddress (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_laddress, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_laddress (m_ctxt); }
  virtual bool gate (function *) { return optimize != 0; }
  virtual unsigned int execute (function *);

}; // class pass_laddress

unsigned int
pass_laddress::execute (function *fun)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, fun)
    {
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (!is_gimple_assign (stmt)
	      || gimple_assign_rhs_code (stmt) != ADDR_EXPR
	      || is_gimple_invariant_address (gimple_assign_rhs1 (stmt)))
	    {
	      gsi_next (&gsi);
	      continue;
	    }

	  /* Lower ADDR_EXPR assignments:
	       _4 = &b[i_9];
	     into
	       _1 = (sizetype) i_9;
	       _7 = _1 * 4;
	       _4 = &b + _7;
	     This ought to aid the vectorizer and expose CSE opportunities.
	  */

	  tree expr = gimple_assign_rhs1 (stmt);
	  poly_int64 bitsize, bitpos;
	  tree base, offset;
	  machine_mode mode;
	  int volatilep = 0, reversep, unsignedp = 0;
	  base = get_inner_reference (TREE_OPERAND (expr, 0), &bitsize,
				      &bitpos, &offset, &mode, &unsignedp,
				      &reversep, &volatilep);
	  gcc_assert (base != NULL_TREE);
	  poly_int64 bytepos = exact_div (bitpos, BITS_PER_UNIT);
	  if (offset != NULL_TREE)
	    {
	      if (maybe_ne (bytepos, 0))
		offset = size_binop (PLUS_EXPR, offset, size_int (bytepos));
	      offset = force_gimple_operand_gsi (&gsi, offset, true, NULL,
						 true, GSI_SAME_STMT);
	      base = build_fold_addr_expr (base);
	      base = force_gimple_operand_gsi (&gsi, base, true, NULL,
					       true, GSI_SAME_STMT);
	      gimple *g = gimple_build_assign (gimple_assign_lhs (stmt),
					      POINTER_PLUS_EXPR, base, offset);
	      gsi_replace (&gsi, g, false);
	    }
	  gsi_next (&gsi);
	}
    }

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_laddress (gcc::context *ctxt)
{
  return new pass_laddress (ctxt);
}
