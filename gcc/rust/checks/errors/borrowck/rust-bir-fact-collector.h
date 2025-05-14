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
// <http://www.gnu.org/licenses/>.

#ifndef RUST_BIR_FACT_COLLECTOR_H
#define RUST_BIR_FACT_COLLECTOR_H

#include "rust-bir-visitor.h"
#include "rust-bir.h"
#include "rust-bir-place.h"
#include "polonius/rust-polonius.h"

namespace Rust {
namespace BIR {

enum class PointPosition : uint8_t
{
  START,
  MID
};

class FactCollector : public Visitor
{
  // Output.
  Polonius::Facts facts;

  // Read-only context.
  const PlaceDB &place_db;
  const BasicBlocks &basic_blocks;
  const PlaceId first_local;
  const location_t location;

  Resolver::TypeCheckContext &tyctx;

  // Collector state.
  BasicBlockId current_bb = ENTRY_BASIC_BLOCK;
  uint32_t current_stmt = 0;
  PlaceId lhs = INVALID_PLACE;

  // PlaceDB is const in this phase, so this is used to generate fresh regions.
  FreeRegion next_fresh_region;
  RegionBinder region_binder{next_fresh_region};

  std::vector<Polonius::Point> cfg_points_all;

  FreeRegions bind_regions (std::vector<TyTy::Region> regions,
			    FreeRegions parent_free_regions)
  {
    return region_binder.bind_regions (regions, parent_free_regions);
  }

  FreeRegions make_fresh_regions (size_t size)
  {
    FreeRegions free_regions;
    for (size_t i = 0; i < size; i++)
      free_regions.push_back (region_binder.get_next_free_region ());

    return free_regions;
  }

public:
  static Polonius::Facts collect (Function &func)
  {
    FactCollector collector (func);
    collector.init_universal_regions (func.universal_regions,
				      func.universal_region_bounds);

    collector.visit_statemensts ();
    collector.visit_places (func.arguments);

    return std::move (collector.facts);
  }

protected: // Constructor and destructor.
  explicit FactCollector (Function &func)
    : place_db (func.place_db), basic_blocks (func.basic_blocks),
      first_local (func.arguments.empty ()
		     ? FIRST_VARIABLE_PLACE
		     : PlaceId{func.arguments.rbegin ()->value + 1}),
      location (func.location), tyctx (*Resolver::TypeCheckContext::get ()),
      next_fresh_region (place_db.peek_next_free_region ())
  {}
  ~FactCollector () = default;

protected: // Main collection entry points (for different categories).
  void init_universal_regions (
    const FreeRegions &universal_regions,
    const decltype (Function::universal_region_bounds) &universal_region_bounds)
  {
    size_t next_loan = place_db.get_loans ().size ();
    facts.universal_region.emplace_back (0);
    facts.placeholder.emplace_back (0, next_loan++);

    for (auto &region : universal_regions)
      {
	facts.universal_region.emplace_back (region.value);
	facts.placeholder.emplace_back (region.value, next_loan++);
	facts.known_placeholder_subset.emplace_back (0, region.value);
      }

    // Copy already collected subset facts, that are universally valid.
    for (auto &bound : universal_region_bounds)
      facts.known_placeholder_subset.emplace_back (bound.first.value,
						   bound.second.value);
  }

  void visit_places (const std::vector<PlaceId> &args)
  {
    for (PlaceId place_id = INVALID_PLACE; place_id.value < place_db.size ();
	 ++place_id.value)
      {
	auto &place = place_db[place_id];

	switch (place.kind)
	  {
	  case Place::VARIABLE:
	  case Place::TEMPORARY:
	    facts.path_is_var.emplace_back (place_id.value, place_id.value);
	    for (auto &region : place.regions)
	      facts.use_of_var_derefs_origin.emplace_back (place_id.value,
							   region.value);

	    // TODO: drop_of_var_derefs_origin
	    break;
	  case Place::FIELD:
	    sanizite_field (place_id);
	    facts.child_path.emplace_back (place_id.value,
					   place.path.parent.value);
	    break;
	  case Place::INDEX:
	    push_subset_all (place.tyty, place.regions,
			     place_db[place.path.parent].regions);
	    facts.child_path.emplace_back (place_id.value,
					   place.path.parent.value);
	    break;
	  case Place::DEREF:
	    sanitize_deref (place_id);
	    facts.child_path.emplace_back (place_id.value,
					   place.path.parent.value);
	    break;
	  case Place::CONSTANT:
	  case Place::INVALID:
	    break;
	  }
      }

    for (PlaceId arg = PlaceId{FIRST_VARIABLE_PLACE.value + 1};
	 arg < first_local; ++arg.value)
      facts.path_assigned_at_base.emplace_back (
	arg.value, get_point (ENTRY_BASIC_BLOCK, 0, PointPosition::START));

    for (PlaceId place = first_local; place.value < place_db.size ();
	 ++place.value)
      {
	if (place_db[place].is_var ())
	  facts.path_moved_at_base.emplace_back (
	    place.value,
	    get_point (ENTRY_BASIC_BLOCK, 0, PointPosition::START));
      }
  }

  void sanitize_deref (PlaceId place_id)
  {
    auto &place = place_db[place_id];
    auto &base = place_db[place.path.parent];

    rust_debug ("\tSanitize deref of %s", base.tyty->as_string ().c_str ());

    FreeRegions regions;
    for (auto it = base.regions.begin () + 1; it != base.regions.end (); ++it)
      {
	regions.push_back (*it);
      }
    push_subset_all (place.tyty, regions, place.regions);
  }
  void sanizite_field (PlaceId place_id)
  {
    auto &place = place_db[place_id];
    auto &base = place_db[place.path.parent];

    rust_debug ("\tSanitize field .%d of %s", place.variable_or_field_index,
		base.tyty->as_string ().c_str ());

    if (base.tyty->is<TyTy::TupleType> ())
      return;
    auto r = Resolver::TypeCheckContext::get ()
	       ->get_variance_analysis_ctx ()
	       .query_field_regions (base.tyty->as<TyTy::ADTType> (), 0,
				     place.variable_or_field_index,
				     base.regions); // FIXME
    push_subset_all (place.tyty, r, place.regions);
  }

  void visit_statemensts ()
  {
    rust_debug ("visit_statemensts");

    for (current_bb = ENTRY_BASIC_BLOCK;
	 current_bb.value < basic_blocks.size (); ++current_bb.value)
      {
	auto &bb = basic_blocks[current_bb];
	for (current_stmt = 0; current_stmt < bb.statements.size ();
	     ++current_stmt)
	  {
	    cfg_points_all.push_back (get_current_point_start ());
	    cfg_points_all.push_back (get_current_point_mid ());

	    add_stmt_to_cfg (current_bb, current_stmt);

	    visit (bb.statements[current_stmt]);
	  }
      }
    current_bb = ENTRY_BASIC_BLOCK;
    current_stmt = 0;
  }

  void visit (const Statement &stmt) override
  {
    switch (stmt.get_kind ())
      {
	case Statement::Kind::ASSIGNMENT: {
	  // TODO: for unwind, must had hadning for non-panic-only assignements
	  issue_write_deep (stmt.get_place ());
	  visit_assignment_expr (stmt.get_place (), stmt.get_expr ());
	  break;
	}
	case Statement::Kind::SWITCH: {
	  issue_read_move (stmt.get_place ());
	  issue_jumps ();
	}
	break;
	case Statement::Kind::GOTO: {
	  issue_jumps ();
	}
	break;
	case Statement::Kind::RETURN: {
	  issue_place_access (RETURN_VALUE_PLACE);
	  issue_locals_dealloc ();
	  break;
	}
	case Statement::Kind::STORAGE_DEAD: {
	  facts.path_moved_at_base.emplace_back (stmt.get_place ().value,
						 get_current_point_mid ());
	  facts.var_defined_at.emplace_back (stmt.get_place ().value,
					     get_current_point_mid ());
	  break;
	}
	case Statement::Kind::STORAGE_LIVE: {
	  issue_write_deep (stmt.get_place (), true);
	  break;
	}
	case Statement::Kind::USER_TYPE_ASCRIPTION: {
	  issue_user_type_constraints (stmt.get_place (), stmt.get_type ());
	  break;
	}
	case Statement::Kind::FAKE_READ: {
	  issue_place_access (stmt.get_place ());
	  break;
	}
      }
  }

  void visit_assignment_expr (PlaceId lhs, AbstractExpr &expr)
  {
    this->lhs = lhs;
    expr.accept_vis (*this);
    this->lhs = INVALID_PLACE;
  }

  void visit (const InitializerExpr &expr) override
  {
    sanitize_constrains_at_init (lhs);

    for (auto init_value : expr.get_values ())
      issue_read_move (init_value);
  }

  void visit (const Operator<1> &expr) override
  {
    sanitize_constrains_at_init (lhs);
    issue_read_move (expr.get_operand<0> ());
  }

  void visit (const Operator<2> &expr) override
  {
    sanitize_constrains_at_init (lhs);
    issue_read_move (expr.get_operand<0> ());
    issue_read_move (expr.get_operand<1> ());
  }

  void visit (const BorrowExpr &expr) override
  {
    rust_debug ("\t_%u = BorrowExpr(_%u)", lhs.value - 1,
		expr.get_place ().value - 1);

    auto loan = place_db.get_loan (expr.get_loan_id ());

    auto &base_place = place_db[expr.get_place ()];
    auto &ref_place = place_db[lhs];

    issue_place_access (expr.get_place ());

    // See compiler/rustc_borrowck/src/type_check/mod.rs:add_reborrow_constraint
    if (base_place.kind == Place::DEREF)
      {
	// Reborrow

	auto &main_loan_place = place_db[base_place.path.parent];
	if (loan.mutability == Mutability::Mut)
	  {
	    if (!main_loan_place.tyty->as<TyTy::ReferenceType> ()
		   ->is_mutable ())
	      rust_error_at (location,
			     "Cannot reborrow immutable borrow as mutable");
	    issue_loan (expr.get_origin (), expr.get_loan_id ());
	  }

	push_subset (main_loan_place.regions[0], {expr.get_origin ()});
      }
    else
      {
	issue_loan (expr.get_origin (), expr.get_loan_id ());
      }

    auto loan_regions = base_place.regions.prepend ({expr.get_origin ()});
    push_subset (ref_place.tyty, loan_regions, ref_place.regions);
  }

  void visit (const Assignment &expr) override
  {
    rust_debug ("\t_%u = Assignment(_%u) at %u:%u", lhs.value - 1,
		expr.get_rhs ().value - 1, current_bb.value, current_stmt);

    issue_read_move (expr.get_rhs ());
    push_place_subset (lhs, expr.get_rhs ());
  }

  void visit (const CallExpr &expr) override
  {
    rust_debug ("\t_%u = CallExpr(_%u)", lhs.value - 1,
		expr.get_callable ().value - 1);

    auto &return_place = place_db[lhs];
    auto &callable_place = place_db[expr.get_callable ()];
    auto callable_ty = callable_place.tyty->as<TyTy::CallableTypeInterface> ();

    issue_read_move (expr.get_callable ());

    // Each call needs unique regions.
    auto call_regions = make_fresh_regions (callable_place.regions.size ());

    for (size_t i = 0; i < expr.get_arguments ().size (); ++i)
      {
	auto arg = expr.get_arguments ().at (i);
	auto arg_regions
	  = bind_regions (Resolver::TypeCheckContext::get ()
			    ->get_variance_analysis_ctx ()
			    .query_type_regions (
			      callable_ty->get_param_type_at (i)),
			  call_regions);
	issue_read_move (arg);
	push_subset (place_db[arg].tyty, place_db[arg].regions, arg_regions);
      }

    // sanitize return regions
    sanitize_constrains_at_init (lhs);

    auto return_regions
      = bind_regions (Resolver::TypeCheckContext::get ()
			->get_variance_analysis_ctx ()
			.query_type_regions (
			  callable_ty->as<TyTy::FnType> ()->get_return_type ()),
		      call_regions);
    push_subset (return_place.tyty, return_regions, return_place.regions);

    issue_jumps ();
  }

protected: // Statement visitor helpers
  WARN_UNUSED_RESULT const BasicBlock &get_current_bb () const
  {
    return basic_blocks[current_bb];
  }

  WARN_UNUSED_RESULT static Polonius::Point
  get_point (BasicBlockId bb, uint32_t stmt, PointPosition pos)
  {
    Polonius::Point point = 0;
    point |= (bb.value << 16);
    point |= (stmt << 1);
    point |= (static_cast<uint8_t> (pos) & 1);
    return point;
  }

  WARN_UNUSED_RESULT Polonius::Point get_current_point_start () const
  {
    return get_point (current_bb, current_stmt, PointPosition::START);
  }

  WARN_UNUSED_RESULT Polonius::Point get_current_point_mid () const
  {
    return get_point (current_bb, current_stmt, PointPosition::MID);
  }

  void add_stmt_to_cfg (BasicBlockId bb, uint32_t stmt)
  {
    if (stmt != 0)
      {
	facts.cfg_edge.emplace_back (get_point (bb, stmt - 1,
						PointPosition::MID),
				     get_point (bb, stmt,
						PointPosition::START));
      }

    facts.cfg_edge.emplace_back (get_point (bb, stmt, PointPosition::START),
				 get_point (bb, stmt, PointPosition::MID));
  }

protected: // Generic BIR operations.
  void issue_jumps ()
  {
    for (auto succ : get_current_bb ().successors)
      facts.cfg_edge.emplace_back (get_current_point_mid (),
				   get_point (succ, 0, PointPosition::START));
  }

  /* Shallow r/w access */
  void issue_place_access (PlaceId place_id)
  {
    auto &place = place_db[place_id];

    if (place.is_constant ())
      return;

    if (place_id != RETURN_VALUE_PLACE)
      facts.path_accessed_at_base.emplace_back (place_id.value,
						get_current_point_mid ());

    if (place.is_var ())
      facts.var_used_at.emplace_back (place_id.value, get_current_point_mid ());
    else if (place.is_path ())
      {
	facts.var_used_at.emplace_back (place_db.get_var (place_id).value,
					get_current_point_mid ());
      }
  }

  /** Deep read access, which consumes the place. */
  void issue_read_move (PlaceId place_id)
  {
    auto &place = place_db[place_id];

    issue_place_access (place_id);
    if (place.should_be_moved ())
      {
	issue_move (place_id);
      }
    else
      {
	check_read_for_conflicts (place_id);
      }
  }

  void issue_write_deep (PlaceId place_id, bool is_init = false)
  {
    auto &place = place_db[place_id];
    rust_assert (place.is_lvalue () || place.is_rvalue ());

    if (place.is_var ())
      facts.var_defined_at.emplace_back (place_id.value,
					 get_current_point_mid ());

    if (!is_init)
      {
	facts.path_assigned_at_base.emplace_back (place_id.value,
						  get_current_point_mid ());
	check_write_for_conflict (place_id);
	kill_borrows_for_place (place_id);
      }
  }

  void issue_move (PlaceId place_id, bool initial = false)
  {
    if (!place_db[place_id].should_be_moved ())
      return;

    facts.path_moved_at_base.emplace_back (place_id.value,
					   initial
					     ? get_point (ENTRY_BASIC_BLOCK, 0,
							  PointPosition::START)
					     : get_current_point_mid ());

    check_move_behind_reference (place_id);

    if (!initial)
      {
	check_write_for_conflict (place_id);
	kill_borrows_for_place (place_id);
      }
  }

  void issue_loan (Polonius::Origin origin, LoanId loan_id)
  {
    facts.loan_issued_at.emplace_back (origin, loan_id.value,
				       get_current_point_mid ());

    check_for_borrow_conficts (place_db.get_loan (loan_id).place, loan_id,
			       place_db.get_loan (loan_id).mutability);
  }

  void issue_locals_dealloc ()
  {
    for (LoanId loan_id = {0}; loan_id.value < place_db.get_loans ().size ();
	 ++loan_id.value)
      {
	auto &loan = place_db.get_loan (loan_id);
	auto loaned_var_id = place_db.get_var (loan.place);
	if (place_db[loaned_var_id].tyty->is<TyTy::ReferenceType> ())
	  continue;
	if (loaned_var_id >= first_local)
	  facts.loan_invalidated_at.emplace_back (get_current_point_start (),
						  loan_id.value);
      }
  }

  void issue_user_type_constraints (PlaceId place_id, TyTy::BaseType *type)
  {
    auto user_regions = Resolver::TypeCheckContext::get ()
			  ->get_variance_analysis_ctx ()
			  .query_type_regions (type);
    push_subset_user (place_db[place_id].tyty, place_db[place_id].regions,
		      user_regions);
  }

  void check_read_for_conflicts (PlaceId place_id)
  {
    place_db.for_each_path_segment (place_id, [&] (PlaceId id) {
      for (auto loan : place_db[id].borrowed_by)
	{
	  if (place_db.get_loan (loan).mutability == Mutability::Mut)
	    {
	      facts.loan_invalidated_at.emplace_back (
		get_current_point_start (), loan.value);
	    }
	}
    });
    place_db.for_each_path_from_root (place_id, [&] (PlaceId id) {
      for (auto loan : place_db[id].borrowed_by)
	{
	  if (place_db.get_loan (loan).mutability == Mutability::Mut)
	    {
	      facts.loan_invalidated_at.emplace_back (
		get_current_point_start (), loan.value);
	    }
	}
    });
  }

  void check_write_for_conflict (PlaceId place_id)
  {
    place_db.for_each_path_segment (place_id, [&] (PlaceId id) {
      for (auto loan : place_db[id].borrowed_by)
	facts.loan_invalidated_at.emplace_back (get_current_point_start (),
						loan.value);
    });
    place_db.for_each_path_from_root (place_id, [&] (PlaceId id) {
      for (auto loan : place_db[id].borrowed_by)
	facts.loan_invalidated_at.emplace_back (get_current_point_start (),
						loan.value);
    });
  }

  void check_for_borrow_conficts (PlaceId place_id, LoanId loan,
				  Mutability mutability)
  {
    place_db.for_each_path_segment (place_id, [&] (PlaceId id) {
      for (auto other_loan : place_db[id].borrowed_by)
	{
	  if (mutability == Mutability::Imm
	      && place_db.get_loan (other_loan).mutability == Mutability::Imm)
	    continue;
	  else
	    facts.loan_invalidated_at.emplace_back (get_current_point_start (),
						    other_loan.value);
	}
    });

    place_db.for_each_path_from_root (place_id, [&] (PlaceId id) {
      for (auto other_loan : place_db[id].borrowed_by)
	{
	  if (mutability == Mutability::Imm
	      && place_db.get_loan (other_loan).mutability == Mutability::Imm)
	    continue;
	  else
	    facts.loan_invalidated_at.emplace_back (get_current_point_start (),
						    other_loan.value);
	}
    });
  }

  void check_move_behind_reference (PlaceId place_id)
  {
    place_db.for_each_path_segment (place_id, [&] (PlaceId id) {
      if (id == place_id)
	return;
      if (place_db[id].kind == Place::DEREF)
	rust_error_at (location, "Cannot move from behind a reference.");
    });
  }

  void kill_borrows_for_place (PlaceId place_id)
  {
    auto &place = place_db[place_id];
    for (auto loan : place.borrowed_by)
      {
	// TODO: this is more complicated, see
	// compiler/rustc_borrowck/src/constraint_generation.rs:176
	facts.loan_killed_at.emplace_back (loan.value,
					   get_current_point_mid ());
      }
  }

protected: // Subset helpers.
  void push_subset (FreeRegion lhs, FreeRegion rhs)
  {
    rust_debug ("\t\tpush_subset: '?%lu: '?%lu", (unsigned long) lhs.value,
		(unsigned long) rhs.value);

    facts.subset_base.emplace_back (lhs.value, rhs.value,
				    get_current_point_mid ());
  }

  void push_subset_all (FreeRegion lhs, FreeRegion rhs)
  {
    rust_debug ("\t\tpush_subset_all: '?%lu: '?%lu", (unsigned long) lhs.value,
		(unsigned long) rhs.value);

    for (auto point : cfg_points_all)
      facts.subset_base.emplace_back (lhs.value, rhs.value, point);
  }

  void push_subset (Variance variance, FreeRegion lhs, FreeRegion rhs)
  {
    if (variance.is_covariant ())
      push_subset (lhs, rhs);
    else if (variance.is_contravariant ())
      push_subset (rhs, lhs);
    else if (variance.is_invariant ())
      {
	push_subset (lhs, rhs);
	push_subset (rhs, lhs);
      }
  }

  void push_subset_all (Variance variance, FreeRegion lhs, FreeRegion rhs)
  {
    if (variance.is_covariant ())
      push_subset_all (lhs, rhs);
    else if (variance.is_contravariant ())
      push_subset_all (rhs, lhs);
    else if (variance.is_invariant ())
      {
	push_subset_all (lhs, rhs);
	push_subset_all (rhs, lhs);
      }
  }

  void push_place_subset (PlaceId lhs, PlaceId rhs)
  {
    auto &lhs_place = place_db[lhs];
    auto &rhs_place = place_db[rhs];

    push_subset (lhs_place.tyty, rhs_place.regions, lhs_place.regions);
  }

  void push_subset (TyTy::BaseType *type, FreeRegions lhs, FreeRegions rhs)
  {
    auto variances = Resolver::TypeCheckContext::get ()
		       ->get_variance_analysis_ctx ()
		       .query_type_variances (type);
    rust_assert (lhs.size () == rhs.size ());
    rust_assert (lhs.size () == variances.size ());
    for (size_t i = 0; i < lhs.size (); ++i)
      push_subset (variances[i], lhs[i], rhs[i]);
  }

  void push_subset_all (TyTy::BaseType *type, FreeRegions lhs, FreeRegions rhs)
  {
    auto variances = Resolver::TypeCheckContext::get ()
		       ->get_variance_analysis_ctx ()
		       .query_type_variances (type);
    rust_assert (lhs.size () == rhs.size ());
    rust_assert (lhs.size () == variances.size ());
    for (size_t i = 0; i < lhs.size (); ++i)
      push_subset_all (variances[i], lhs[i], rhs[i]);
  }

  void push_subset_user (TyTy::BaseType *type, FreeRegions free_regions,
			 std::vector<TyTy::Region> user_regions)
  {
    auto variances = Resolver::TypeCheckContext::get ()
		       ->get_variance_analysis_ctx ()
		       .query_type_variances (type);
    rust_assert (free_regions.size () == user_regions.size ());
    rust_assert (free_regions.size () == variances.size ());

    for (size_t i = 0; i < free_regions.size (); ++i)
      {
	if (user_regions[i].is_named ())
	  push_subset (variances[i], free_regions[i],
		       {Polonius::Origin (user_regions[i].get_index ())});
	else if (user_regions[i].is_anonymous ())
	  {
	    // IGNORE
	  }
	else
	  rust_internal_error_at (UNKNOWN_LOCATION, "Unexpected region type");
      }
  }

  /**
   * Apply type and lifetime bounds
   *
   * For a place we have a list of fresh regions. We need to apply constraints
   * from type definition to it. First `n` regions belong to the lifetime
   * parameters of the type. The rest are flatten lifetime parameters of the
   * type arguments. We walk the type arguments with a offset
   */
  void sanitize_constrains_at_init (PlaceId place_id)
  {
    auto &place = place_db[place_id];

    rust_debug ("\tSanitize constraints of %s",
		place.tyty->as_string ().c_str ());

    if (auto generic = place.tyty->try_as<TyTy::SubstitutionRef> ())
      {
	auto &regions = place.regions;
	auto region_end = sanitize_constraints (*generic, 0, regions);
	rust_assert (region_end == regions.size ());
      }
    else if (place.tyty->is<TyTy::ReferenceType> ())
      {
	for (auto &region : place.regions)
	  {
	    if (region != place.regions[0])
	      push_subset (region, place.regions[0]);
	  }
      }
  }

  size_t sanitize_constraints (const TyTy::BaseType *type, size_t region_start,
			       const FreeRegions &regions)
  {
    switch (type->get_kind ())
      {
      case TyTy::ADT:
	return sanitize_constraints (type->as<const TyTy::ADTType> (),
				     region_start, regions);
      case TyTy::STR:
	return region_start;
      case TyTy::REF:
	return 1
	       + sanitize_constraints (
		 type->as<const TyTy::ReferenceType> ()->get_base (),
		 region_start, regions);
      case TyTy::POINTER:
	return sanitize_constraints (
	  type->as<const TyTy::PointerType> ()->get_base (), region_start,
	  regions);
      case TyTy::ARRAY:
	return sanitize_constraints (
	  type->as<const TyTy::ArrayType> ()->get_element_type (), region_start,
	  regions);
      case TyTy::SLICE:
	return sanitize_constraints (
	  type->as<const TyTy::SliceType> ()->get_element_type (), region_start,
	  regions);
      case TyTy::FNDEF:
	case TyTy::TUPLE: {
	  for (auto &field : type->as<const TyTy::TupleType> ()->get_fields ())
	    sanitize_constraints (field.get_tyty (), region_start, regions);
	}
	break;
      case TyTy::FNPTR:
      case TyTy::PROJECTION:
	return sanitize_constraints (*type->as<const TyTy::SubstitutionRef> (),
				     region_start, regions);
      case TyTy::BOOL:
      case TyTy::CHAR:
      case TyTy::INT:
      case TyTy::UINT:
      case TyTy::FLOAT:
      case TyTy::USIZE:
      case TyTy::ISIZE:
      case TyTy::NEVER:
      case TyTy::DYNAMIC:
      case TyTy::CLOSURE:
      case TyTy::ERROR:
	return region_start;
      case TyTy::PLACEHOLDER:
      case TyTy::INFER:
      case TyTy::PARAM:
      case TyTy::OPAQUE:
	rust_unreachable ();
      }
    rust_unreachable ();
  }

  size_t sanitize_constraints (const TyTy::SubstitutionRef &type,
			       size_t region_start, const FreeRegions &regions)
  {
    for (auto constr : type.get_region_constraints ().region_region)
      {
	rust_assert (constr.first.is_early_bound ());
	rust_assert (constr.second.is_early_bound ());
	auto lhs = constr.first.get_index () + region_start;
	auto rhs = constr.second.get_index () + region_start;
	push_subset (regions[lhs], regions[rhs]);
      }

    size_t region_end = region_start + type.get_num_lifetime_params ();

    /*
     * For type `Foo<'a, T1, T2>`, where `T1 = &'b Vec<&'c i32>` and `T2 = &'d
     * i32 the regions are `['a, 'b, 'c, 'd]`. The ranges
     */
    std::vector<size_t> type_param_region_ranges;
    type_param_region_ranges.push_back (region_end);

    for (auto type_param : type.get_substs ())
      {
	TyTy::SubstitutionArg arg = TyTy::SubstitutionArg::error ();
	bool ok = type.get_used_arguments ().get_argument_for_symbol (
	  type_param.get_param_ty (), &arg);
	rust_assert (ok);
	region_end
	  = sanitize_constraints (arg.get_tyty (), region_end, regions);
	type_param_region_ranges.push_back (region_end);
      }

    /*
     * For constrain of form: `T: 'a` push outlives with all in range
     * `indexof(T)..(indexof(T) + 1)`
     */
    for (auto constr : type.get_region_constraints ().type_region)
      {
	auto type_param_index_opt
	  = type.get_used_arguments ().find_symbol (*constr.first);
	rust_assert (type_param_index_opt.has_value ());
	size_t type_param_index = type_param_index_opt.value ();

	for (size_t i = type_param_region_ranges[type_param_index];
	     i < type_param_region_ranges[type_param_index + 1]; ++i)
	  {
	    push_subset (regions[i],
			 regions[constr.second.get_index () + region_start]);
	  }
      }

    return region_end;
  }
}; // namespace BIR

} // namespace BIR
} // namespace Rust

#endif // RUST_BIR_FACT_COLLECTOR_H
