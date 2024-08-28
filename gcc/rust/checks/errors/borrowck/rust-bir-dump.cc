// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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
// <http://www.gnu.org/licenses/>

#include "rust-system.h"
#include "rust-bir-dump.h"
#include "rust-diagnostics.h"

namespace Rust {
namespace BIR {

constexpr auto indentation = "    ";

std::string
get_tyty_name (TyTy::BaseType *tyty)
{
  if (tyty)
    return tyty->get_name ();
  return "unknown";
}

template <typename T, typename FN>
void
print_comma_separated (std::ostream &stream, const std::vector<T> &collection,
		       FN printer)
{
  if (collection.empty ())
    return;
  printer (collection[0]);
  for (auto it = collection.begin () + 1; it != collection.end (); ++it)
    {
      stream << ", ";
      printer (*it);
    }
}

void
renumber_places (const Function &func, std::vector<PlaceId> &place_map)
{
  // Renumbering places to avoid gaps in the place id space.
  // This is needed to match MIR's shape.
  PlaceId next_out_id = INVALID_PLACE;

  for (PlaceId in_id = FIRST_VARIABLE_PLACE;
       in_id.value < func.place_db.size (); ++in_id.value)
    {
      const Place &place = func.place_db[in_id];
      if (place.kind == Place::VARIABLE || place.kind == Place::TEMPORARY)
	{
	  place_map[in_id.value] = next_out_id;
	  ++next_out_id.value;
	}

      else
	place_map[in_id.value] = INVALID_PLACE;
    }
}

void
simplify_cfg (Function &func, IndexVec<BasicBlockId, BasicBlockId> &bb_fold_map)
{
  // The BIR builder can generate many useless basic blocks, which contain only
  // a goto.
  // For actual borrow-checking, the folding has little value.

  bool stabilized = false;
  while (!stabilized)
    {
      stabilized = true;
      // BB0 cannot be folded as it is an entry block.
      for (BasicBlockId i = {1}; i.value < func.basic_blocks.size (); ++i.value)
	{
	  const BasicBlock &bb = func.basic_blocks[bb_fold_map[i]];
	  if (bb.statements.empty () && bb.is_goto_terminated ())
	    {
	      auto dst = bb.successors.at (0);
	      if (bb_fold_map[dst] != dst)
		{
		  rust_error_at (
		    UNKNOWN_LOCATION,
		    "BIR DUMP: Cannot fold CFG, because it contains an "
		    "infinite loop with no executable statements.");
		  rust_inform (UNKNOWN_LOCATION,
			       "Continuing with an unfolded CFG.");
		  // Reverting the fold map to the original state.
		  for (BasicBlockId i = ENTRY_BASIC_BLOCK;
		       i.value < bb_fold_map.size (); ++i.value)
		    {
		      bb_fold_map[i] = i;
		    }
		  stabilized = true;
		  break;
		}
	      bb_fold_map[i] = dst;
	      stabilized = false;
	    }
	}
    }
}

void
Dump::go (bool enable_simplify_cfg)
{
  // To avoid mutation of the BIR, we use indirection through bb_fold_map.
  for (BasicBlockId i = ENTRY_BASIC_BLOCK; i.value < bb_fold_map.size ();
       ++i.value)
    {
      bb_fold_map[i] = i;
    }
  for (PlaceId i = INVALID_PLACE; i.value < place_map.size (); ++i.value)
    {
      place_map[i] = i;
    }

  if (enable_simplify_cfg)
    simplify_cfg (func, bb_fold_map);

  // renumber_places (func, place_map);

  stream << "fn " << name << "(";
  print_comma_separated (stream, func.arguments, [this] (PlaceId place_id) {
    stream << "_" << place_map[place_id].value << ": "
	   << get_tyty_name (func.place_db[place_id].tyty);
  });
  stream << ") -> " << get_tyty_name (func.place_db[RETURN_VALUE_PLACE].tyty);
  stream << " {\n";

  // Print locals declaration.
  visit_scope (ROOT_SCOPE);

  // Print BBs.
  for (statement_bb = ENTRY_BASIC_BLOCK;
       statement_bb.value < func.basic_blocks.size (); ++statement_bb.value)
    {
      if (bb_fold_map[statement_bb] != statement_bb)
	continue; // This BB was folded.

      if (func.basic_blocks[statement_bb].statements.empty ()
	  && func.basic_blocks[statement_bb].successors.empty ())
	continue;

      bb_terminated = false;

      BasicBlock &bb = func.basic_blocks[statement_bb];
      stream << "\n";
      stream << indentation << "bb" << bb_fold_map[statement_bb].value
	     << ": {\n";
      size_t i = 0;
      for (auto &stmt : bb.statements)
	{
	  stream << indentation << i++ << indentation;
	  visit (stmt);
	  stream << ";\n";
	}
      if (!bb_terminated)
	stream << indentation << indentation << "goto -> bb"
	       << bb_fold_map[bb.successors.at (0)].value << ";\t\t" << i++
	       << "\n";

      stream << indentation << "}\n";
    }

  stream << "}\n";
}
void
Dump::visit (const Statement &stmt)
{
  statement_place = stmt.get_place ();
  switch (stmt.get_kind ())
    {
      case Statement::Kind::ASSIGNMENT: {
	visit_place (stmt.get_place ());
	stream << " = ";
	stmt.get_expr ().accept_vis (*this);
	break;
      }
    case Statement::Kind::SWITCH:
      stream << "switchInt(";
      visit_move_place (stmt.get_place ());
      stream << ") -> [";
      print_comma_separated (stream, func.basic_blocks[statement_bb].successors,
			     [this] (BasicBlockId succ) {
			       stream << "bb" << bb_fold_map[succ].value;
			     });
      stream << "]";
      bb_terminated = true;
      break;
    case Statement::Kind::RETURN:
      stream << "return";
      bb_terminated = true;
      break;
    case Statement::Kind::GOTO:
      stream
	<< "goto -> bb"
	<< bb_fold_map[func.basic_blocks[statement_bb].successors.at (0)].value;
      bb_terminated = true;
      break;
    case Statement::Kind::STORAGE_DEAD:
      stream << "StorageDead(";
      visit_place (stmt.get_place ());
      stream << ")";
      break;
    case Statement::Kind::STORAGE_LIVE:
      stream << "StorageLive(";
      visit_place (stmt.get_place ());
      stream << ")";
      break;
    case Statement::Kind::USER_TYPE_ASCRIPTION:
      visit_place (stmt.get_place ());
      stream << " = ";
      stream << "UserTypeAscription(";
      stream << get_tyty_name (func.place_db[stmt.get_place ()].tyty);
      stream << ")";
      break;
    case Statement::Kind::FAKE_READ:
      stream << "FakeRead(";
      visit_place (stmt.get_place ());
      stream << ")";
      break;
    default:
      rust_internal_error_at (UNKNOWN_LOCATION, "Unknown statement kind.");
    }
  statement_place = INVALID_PLACE;
}

void
Dump::visit_place (PlaceId place_id)
{
  const Place &place = func.place_db[place_id];
  switch (place.kind)
    {
    case Place::TEMPORARY:
    case Place::VARIABLE:
      stream << "_" << place_map[place_id].value;
      break;
    case Place::DEREF:
      stream << "(";
      stream << "*";
      visit_place (place.path.parent);
      stream << ")";
      break;
    case Place::FIELD:
      stream << "(";
      visit_place (place.path.parent);
      stream << ".";
      stream << place.variable_or_field_index;
      stream << ": " << get_tyty_name (place.tyty) << ")";
      break;
    case Place::INDEX:
      stream << "(";
      visit_place (place.path.parent);
      stream << "[]";
      stream << ": " << get_tyty_name (place.tyty) << ")";
      break;
    case Place::CONSTANT:
      stream << "const " << get_tyty_name (place.tyty);
      break;
    case Place::INVALID:
      if (place_id == INVALID_PLACE)
	stream << "_INVALID";
    }
}

void
Dump::visit_move_place (PlaceId place_id)
{
  const Place &place = func.place_db[place_id];
  if (place.should_be_moved ())
    stream << "move ";
  visit_place (place_id);
}

void
Dump::visit (const BorrowExpr &expr)
{
  stream << "&"
	 << "'?" << expr.get_origin () << " ";
  if (func.place_db.get_loan (expr.get_loan_id ()).mutability
      == Mutability::Mut)
    stream << "mut ";
  visit_place (expr.get_place ());
}

void
Dump::visit_lifetime (PlaceId place_id)
{}

void
Dump::visit (const InitializerExpr &expr)
{
  stream << "{";
  print_comma_separated (stream, expr.get_values (), [this] (PlaceId place_id) {
    visit_move_place (place_id);
  });
  stream << "}";
}

void
Dump::visit (const CallExpr &expr)
{
  stream << "Call(";
  auto maybe_fn_type
    = func.place_db[expr.get_callable ()].tyty->try_as<TyTy::FnType> ();
  if (maybe_fn_type)
    stream << maybe_fn_type->get_identifier ();
  else
    visit_move_place (expr.get_callable ());

  stream << ")(";
  print_comma_separated (stream, expr.get_arguments (),
			 [this] (PlaceId place_id) {
			   visit_move_place (place_id);
			 });
  stream << ") -> [";
  print_comma_separated (stream, func.basic_blocks[statement_bb].successors,
			 [this] (BasicBlockId succ) {
			   stream << "bb" << bb_fold_map[succ].value;
			 });
  stream << "]";
  bb_terminated = true;
}

void
Dump::visit (const Operator<1> &expr)
{
  stream << "Operator(";
  visit_move_place (expr.get_operand<0> ());
  stream << ")";
}

void
Dump::visit (const Operator<2> &expr)
{
  stream << "Operator(";
  visit_move_place (expr.get_operand<0> ());
  stream << ", ";
  visit_move_place (expr.get_operand<1> ());
  stream << ")";
}

void
Dump::visit (const Assignment &expr)
{
  if (func.place_db[expr.get_rhs ()].is_rvalue ())
    visit_move_place (expr.get_rhs ());
  else
    visit_place (expr.get_rhs ());
}

std::ostream &
Dump::indent (size_t depth)
{
  for (size_t i = 0; i < depth; ++i)
    stream << indentation;
  return stream;
}

void
Dump::visit_scope (ScopeId id, size_t depth)
{
  auto scope = func.place_db.get_scope (id);
  if (scope.locals.empty () && scope.children.empty ())
    return;

  if (id.value > 1)
    indent (depth) << "scope " << id.value - 1 << " {\n";

  for (auto &local : scope.locals)
    {
      indent (depth + 1) << "let _";
      stream << place_map[local].value << ": "
	     << get_tyty_name (func.place_db[local].tyty);
      stream << ";\t";

      stream << "[";
      print_comma_separated (stream,
			     func.place_db[local].regions.get_regions (),
			     [this] (FreeRegion region_id) {
			       stream << "'?" << region_id.value;
			     });
      stream << "]\n";
    }
  for (auto &child : scope.children)
    visit_scope (child, (id.value >= 1) ? depth + 1 : depth);

  if (id.value > 1)
    indent (depth) << "}\n";
}

} // namespace BIR
} // namespace Rust
