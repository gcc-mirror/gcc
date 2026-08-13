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
#include "rust-hir-map.h"

namespace Rust {
namespace BIR {
namespace {

struct BlockInitializationState
{
  explicit BlockInitializationState (size_t place_count)
    : maybe_initialized (place_count, false),
      maybe_uninitialized (place_count, false), reachable (false)
  {}

  std::vector<bool> maybe_initialized;
  std::vector<bool> maybe_uninitialized;

  // Whether this block is reached or not.
  bool reachable;
};

struct DropAnalysisResults
{
  std::unordered_set<HirId> dead_drop_hir_ids;
  std::unordered_set<HirId> static_drop_hir_ids;
  std::unordered_set<HirId> conditional_drop_hir_ids;
  std::unordered_map<HirId, HirId> move_sources;
};

static void
set_initialized (BlockInitializationState &state, PlaceId place)
{
  state.maybe_initialized[place.value] = true;
  state.maybe_uninitialized[place.value] = false;
}

static void
set_uninitialized (BlockInitializationState &state, PlaceId place)
{
  state.maybe_initialized[place.value] = false;
  state.maybe_uninitialized[place.value] = true;
}

// This function combines states from incoming blocks.
// Take conditional move as example,
//
//           BB0
//        initialize x
//           /     |
//          v      v
//      BB1      BB2
//     move x   no change
//          \      /
//           v    v
//            BB3
//           Drop(x)
// BB1 -> BB3
// (BB1): maybe_initialized(x) = false
// (BB1): maybe_uninitialized(x) = true
//
// (BB3): maybe_initialized(x) = true || (BB1): maybe_initialized(x) -> true
// (BB3): maybe_uninitialized(x) = false || (BB1): maybe_uninitialized(x) ->
// true
static bool
merge_state (BlockInitializationState &into,
	     const BlockInitializationState &from)
{
  bool changed = false;

  if (!into.reachable)
    {
      into = from;
      return !changed;
    }

  for (size_t i = 0; i < into.maybe_initialized.size (); i++)
    {
      bool maybe_initialized
	= into.maybe_initialized[i] || from.maybe_initialized[i];

      bool maybe_uninitialized
	= into.maybe_uninitialized[i] || from.maybe_uninitialized[i];

      changed |= maybe_initialized != into.maybe_initialized[i];
      changed |= maybe_uninitialized != into.maybe_uninitialized[i];

      into.maybe_initialized[i] = maybe_initialized;
      into.maybe_uninitialized[i] = maybe_uninitialized;
    }

  return changed;
}

static void
update_state_for_statement (Function &function, Statement &statement,
			    BlockInitializationState &state)
{
  PlaceId place = statement.get_place ();

  switch (statement.get_kind ())
    {
    case Statement::Kind::STORAGE_LIVE:
      set_uninitialized (state, place);
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
	      set_uninitialized (state, rhs);
	  }

	set_initialized (state, lhs);
	break;
      }

    case Statement::Kind::DROP:
    case Statement::Kind::STORAGE_DEAD:
      set_uninitialized (state, place);
      break;

    case Statement::Kind::SWITCH:
    case Statement::Kind::RETURN:
    case Statement::Kind::GOTO:
    case Statement::Kind::USER_TYPE_ASCRIPTION:
    case Statement::Kind::FAKE_READ:
      break;
    }
}

static Statement::DropStyle
classify_drop (const BlockInitializationState &state, PlaceId place)
{
  bool maybe_initialized = state.maybe_initialized[place.value];
  bool maybe_uninitialized = state.maybe_uninitialized[place.value];

  if (!maybe_initialized)
    return Statement::DropStyle::DEAD;

  if (!maybe_uninitialized)
    return Statement::DropStyle::STATIC;

  return Statement::DropStyle::CONDITIONAL;
}

// Compute the initialization state at the entry of every reachable block.
static std::vector<BlockInitializationState>
compute_entry_states (Function &function)
{
  size_t place_count = function.place_db.size ();
  size_t block_count = function.basic_blocks.size ();

  std::vector<BlockInitializationState> entry_states;
  entry_states.reserve (block_count);

  for (size_t i = 0; i < block_count; i++)
    entry_states.emplace_back (place_count);

  BlockInitializationState &entry_state = entry_states[ENTRY_BASIC_BLOCK.value];

  entry_state.reachable = true;

  // All places start uninitialized, except function arguments.
  for (size_t i = 0; i < place_count; i++)
    entry_state.maybe_uninitialized[i] = true;

  for (PlaceId argument : function.arguments)
    set_initialized (entry_state, argument);

  std::vector<BasicBlockId> worklist;
  std::vector<bool> queued (block_count, false);

  worklist.push_back (ENTRY_BASIC_BLOCK);
  queued[ENTRY_BASIC_BLOCK.value] = true;

  // Propagate block states until the last block.
  while (!worklist.empty ())
    {
      BasicBlockId block_id = worklist.back ();
      worklist.pop_back ();
      queued[block_id.value] = false;

      BlockInitializationState state = entry_states[block_id.value];
      BasicBlock &block = function.basic_blocks[block_id];

      for (Statement &statement : block.statements)
	update_state_for_statement (function, statement, state);

      for (BasicBlockId successor : block.successors)
	{
	  bool state_changed
	    = merge_state (entry_states[successor.value], state);

	  if (state_changed && !queued[successor.value])
	    {
	      worklist.push_back (successor);
	      queued[successor.value] = true;
	    }
	}
    }
  return entry_states;
}

static void
record_drop_for_backend (const Function &function, PlaceId place,
			 Statement::DropStyle drop_style,
			 DropAnalysisResults &results)
{
  const Place &dropped_place = function.place_db[place];

  if (dropped_place.kind != Place::VARIABLE)
    return;

  auto hir_id = Analysis::Mappings::get ().lookup_node_to_hir (
    static_cast<NodeId> (dropped_place.variable_or_field_index));

  if (!hir_id.has_value ())
    return;

  switch (drop_style)
    {
    case Statement::DropStyle::UNCLASSIFIED:
      break;

    case Statement::DropStyle::DEAD:
      results.dead_drop_hir_ids.insert (hir_id.value ());
      break;

    case Statement::DropStyle::STATIC:
      results.static_drop_hir_ids.insert (hir_id.value ());
      break;

    case Statement::DropStyle::CONDITIONAL:
      results.conditional_drop_hir_ids.insert (hir_id.value ());
      break;
    }
}

// Walk each reachable block forward from its stable entry state and classify
// its Drop statements.
static void
annotate_drop_statements (
  Function &function, const std::vector<BlockInitializationState> &entry_states,
  DropAnalysisResults &results)
{
  const size_t block_count = function.basic_blocks.size ();

  for (size_t i = 0; i < block_count; i++)
    {
      BlockInitializationState state = entry_states[i];

      if (!state.reachable)
	continue;

      BasicBlockId block_id = {static_cast<uint32_t> (i)};
      BasicBlock &block = function.basic_blocks[block_id];

      for (Statement &statement : block.statements)
	{
	  const auto &move_site = statement.get_move_site ();
	  if (statement.get_kind () == Statement::Kind::ASSIGNMENT
	      && move_site.has_value ())
	    {
	      AbstractExpr &expr = statement.get_expr ();
	      if (expr.get_kind () == ExprKind::ASSIGNMENT)
		{
		  PlaceId rhs = static_cast<Assignment &> (expr).get_rhs ();
		  const Place &rhs_place = function.place_db[rhs];
		  if (rhs_place.kind == Place::VARIABLE
		      && rhs_place.should_be_moved ())
		    {
		      auto hirid
			= Analysis::Mappings::get ().lookup_node_to_hir (
			  static_cast<NodeId> (
			    rhs_place.variable_or_field_index));
		      if (hirid.has_value ())
			results.move_sources[move_site.value ()]
			  = hirid.value ();
		    }
		}
	    }

	  // A Drop is classified using the state before it executes.
	  if (statement.get_kind () == Statement::Kind::DROP)
	    {
	      PlaceId place = statement.get_place ();
	      Statement::DropStyle drop_style = classify_drop (state, place);

	      statement.set_drop_style (drop_style);

	      record_drop_for_backend (function, place, drop_style, results);
	    }

	  // Update the state for the following statement.
	  update_state_for_statement (function, statement, state);
	}
    }
}

} // namespace

DropAnalysis &
DropAnalysis::get ()
{
  static DropAnalysis instance;
  return instance;
}

void
DropAnalysis::clear ()
{
  definitely_dead.clear ();
  conditionally_dropped.clear ();
  move_sources.clear ();
}

bool
DropAnalysis::is_definitely_dead (HirId id) const
{
  return definitely_dead.find (id) != definitely_dead.end ();
}

bool
DropAnalysis::needs_drop_flag (HirId id) const
{
  return conditionally_dropped.find (id) != conditionally_dropped.end ();
}

bool
DropAnalysis::lookup_move_source (HirId move_site, HirId *source) const
{
  auto it = move_sources.find (move_site);
  if (it == move_sources.end ())
    return false;

  *source = it->second;
  return true;
}

void
DropAnalysis::analyze (Function &function)
{
  std::vector<BlockInitializationState> entry_states
    = compute_entry_states (function);

  DropAnalysisResults results;
  annotate_drop_statements (function, entry_states, results);

  // A local is definitely dead only when all of its Drops are dead.
  for (HirId hir_id : results.dead_drop_hir_ids)
    if (results.static_drop_hir_ids.find (hir_id)
	  == results.static_drop_hir_ids.end ()
	&& results.conditional_drop_hir_ids.find (hir_id)
	     == results.conditional_drop_hir_ids.end ())
      definitely_dead.insert (hir_id);

  conditionally_dropped.insert (results.conditional_drop_hir_ids.begin (),
				results.conditional_drop_hir_ids.end ());
  move_sources.insert (results.move_sources.begin (),
		       results.move_sources.end ());
}

} // namespace BIR
} // namespace Rust
