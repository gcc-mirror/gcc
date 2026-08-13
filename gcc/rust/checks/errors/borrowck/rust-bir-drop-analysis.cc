// Copyright (C) 2026 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-bir-drop-analysis.h"
#include "rust-bir.h"

#include <unordered_set>

namespace Rust {
namespace BIR {

namespace {

struct BasicBlockIdHash
{
  size_t operator() (BasicBlockId id) const
  {
    return std::hash<uint32_t> () (id.value);
  }
};

} // namespace

void
DropAnalysis::analyze (Function &function)
{
  std::vector<BasicBlockId> block_order;
  std::unordered_set<BasicBlockId, BasicBlockIdHash> visited;

  BasicBlockId current = ENTRY_BASIC_BLOCK;

  while (current != INVALID_BB)
    {
      // A repeated block indicates a cycle in straight-line control flow.
      if (!visited.insert (current).second)
	return;

      block_order.push_back (current);

      const BasicBlock &block = function.basic_blocks[current];

      if (block.successors.empty ())
	break;

      if (block.successors.size () != 1)
	return;

      current = block.successors.front ();
    }

  std::vector<bool> initialized (function.place_db.size (), false);

  for (PlaceId argument : function.arguments)
    initialized[argument.value] = true;

  for (BasicBlockId block_id : block_order)
    {
      BasicBlock &block = function.basic_blocks[block_id];

      for (Statement &statement : block.statements)
	{
	  PlaceId place = statement.get_place ();

	  switch (statement.get_kind ())
	    {
	    case Statement::Kind::STORAGE_LIVE:
	      initialized[place.value] = false;
	      break;

	    case Statement::Kind::ASSIGNMENT:
	      {
		PlaceId lhs = place;
		AbstractExpr &expr = statement.get_expr ();

		if (expr.get_kind () == ExprKind::ASSIGNMENT)
		  {
		    PlaceId rhs = static_cast<Assignment &> (expr).get_rhs ();
		    const Place &rhs_place = function.place_db[rhs];

		    if (rhs_place.kind == Place::VARIABLE
			&& rhs_place.should_be_moved ())
		      initialized[rhs.value] = false;
		  }

		initialized[lhs.value] = true;
		break;
	      }

	    case Statement::Kind::DROP:
	      statement.set_drop_style (initialized[place.value]
					  ? Statement::DropStyle::STATIC
					  : Statement::DropStyle::DEAD);

	      initialized[place.value] = false;
	      break;

	    case Statement::Kind::STORAGE_DEAD:
	      initialized[place.value] = false;
	      break;

	    case Statement::Kind::SWITCH:
	    case Statement::Kind::RETURN:
	    case Statement::Kind::GOTO:
	    case Statement::Kind::USER_TYPE_ASCRIPTION:
	    case Statement::Kind::FAKE_READ:
	      break;
	    }
	}
    }
}

} // namespace BIR
} // namespace Rust
