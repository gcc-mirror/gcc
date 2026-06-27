// Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

#include "rust-unused-checker.h"
#include "rust-hir-expr.h"
#include "rust-hir-generic-param.h"
#include "rust-hir-item.h"
#include "rust-hir-pattern.h"

#include "options.h"
#include "rust-keyword-values.h"
#include "rust-attribute-values.h"
#include "rust-rib.h"

namespace Rust {
namespace Analysis {
UnusedChecker::UnusedChecker ()
  : nr_context (Resolver2_0::FinalizedNameResolutionContext::get ()),
    mappings (Analysis::Mappings::get ()), unused_context (UnusedContext ())
{}
void
UnusedChecker::go (HIR::Crate &crate)
{
  UnusedCollector collector (unused_context);
  collector.go (crate);
  for (auto &item : crate.get_items ())
    item->accept_vis (*this);
}

bool
is_snake_case (Identifier identifier)
{
  auto s = identifier.as_string ();
  return std::all_of (s.begin (), s.end (), [] (unsigned char c) {
    return ISLOWER (c) || ISDIGIT (c) || c == '_';
  });
}

void
UnusedChecker::visit (HIR::ConstantItem &item)
{
  std::string var_name = item.get_identifier ().as_string ();
  if (var_name == "_" && item.get_visibility ().is_public ())
    rust_warning_at (item.get_locus (), OPT_Wunused_variable,
		     "visibility qualifier on a %<const _%> item is unused");
}

void
UnusedChecker::visit (HIR::StaticItem &item)
{
  std::string var_name = item.get_identifier ().as_string ();
  if (!std::all_of (var_name.begin (), var_name.end (), [] (unsigned char c) {
	return ISUPPER (c) || ISDIGIT (c) || c == '_';
      }))
    rust_warning_at (item.get_locus (), OPT_Wunused_variable,
		     "static variable %qs should have an upper case name",
		     var_name.c_str ());
}

void
UnusedChecker::visit (HIR::TraitItemFunc &item)
{
  // TODO: check trait item functions if they are not derived.
}
void
UnusedChecker::visit (HIR::IdentifierPattern &pattern)
{
  std::string var_name = pattern.get_identifier ().as_string ();
  auto id = pattern.get_mappings ().get_hirid ();
  if (!unused_context.is_variable_used (id)
      && var_name != Values::Keywords::SELF && var_name[0] != '_')
    rust_warning_at (pattern.get_locus (), OPT_Wunused_variable,
		     "unused variable %qs",
		     pattern.get_identifier ().as_string ().c_str ());

  if (pattern.is_mut () && !unused_context.is_mut_used (id)
      && var_name != Values::Keywords::SELF && var_name[0] != '_')
    rust_warning_at (pattern.get_locus (), OPT_Wunused_variable,
		     "unused mut %qs",
		     pattern.get_identifier ().as_string ().c_str ());

  if (!is_snake_case (pattern.get_identifier ()))
    rust_warning_at (pattern.get_locus (), OPT_Wunused_variable,
		     "variable %qs should have a snake case name",
		     var_name.c_str ());
}

void
UnusedChecker::visit (HIR::AssignmentExpr &expr)
{
  const auto &lhs = expr.get_lhs ();
  auto var_name = lhs.to_string ();
  NodeId ast_node_id = lhs.get_mappings ().get_nodeid ();
  if (auto def_id
      = nr_context.lookup (ast_node_id, Resolver2_0::Namespace::Values))
    {
      if (auto id = mappings.lookup_node_to_hir (*def_id))
	{
	  if (unused_context.is_variable_assigned (
		*id, lhs.get_mappings ().get_hirid ())
	      && var_name[0] != '_')
	    rust_warning_at (lhs.get_locus (), OPT_Wunused_variable,
			     "unused assignment %qs", var_name.c_str ());
	}
    }
}

void
UnusedChecker::visit (HIR::StructPatternFieldIdent &pattern)
{
  std::string var_name = pattern.get_identifier ().as_string ();
  auto id = pattern.get_mappings ().get_hirid ();
  if (!unused_context.is_variable_used (id)
      && var_name != Values::Keywords::SELF && var_name[0] != '_')
    rust_warning_at (pattern.get_locus (), OPT_Wunused_variable,
		     "unused variable %qs",
		     pattern.get_identifier ().as_string ().c_str ());

  if (pattern.is_mut () && !unused_context.is_mut_used (id)
      && var_name != Values::Keywords::SELF && var_name[0] != '_')
    rust_warning_at (pattern.get_locus (), OPT_Wunused_variable,
		     "unused mut %qs",
		     pattern.get_identifier ().as_string ().c_str ());
}

void
UnusedChecker::visit (HIR::EmptyStmt &stmt)
{
  rust_warning_at (stmt.get_locus (), OPT_Wunused_variable,
		   "unnecessary trailing semicolons");
}

void
UnusedChecker::visit (HIR::Function &fct)
{
  if (!is_snake_case (fct.get_function_name ()))
    rust_warning_at (fct.get_locus (), OPT_Wunused_variable,
		     "function %qs should have a snake case name",
		     fct.get_function_name ().as_string ().c_str ());

  // The no_mangle_generic_items lint: a generic function cannot be exported
  // with a fixed symbol, so `#[no_mangle]`/`#[export_name]` has no effect.
  if (fct.has_generics ())
    for (auto &attr : fct.get_outer_attrs ())
      {
	auto name = attr.get_path ().as_string ();
	if (name == "no_mangle" || name == "export_name")
	  {
	    rust_warning_at (fct.get_locus (), OPT_Wattributes,
			     "generic functions must be mangled, %qs has no "
			     "effect",
			     name.c_str ());
	    break;
	  }
      }
  walk (fct);
}

void
UnusedChecker::visit (HIR::Module &mod)
{
  if (!is_snake_case (mod.get_module_name ()))
    rust_warning_at (mod.get_locus (), OPT_Wunused_variable,
		     "module %qs should have a snake case name",
		     mod.get_module_name ().as_string ().c_str ());
  walk (mod);
}

void
UnusedChecker::visit (HIR::LifetimeParam &lft)
{
  if (!is_snake_case (lft.get_lifetime ().get_name ()))
    rust_warning_at (lft.get_locus (), OPT_Wunused_variable,
		     "lifetime %qs should have a snake case name",
		     lft.get_lifetime ().get_name ().c_str ());
  walk (lft);
}

void
UnusedChecker::visit (HIR::ExternBlock &block)
{
  if (!block.has_abi ())
    rust_warning_at (block.get_locus (), OPT_Wunused_variable,
		     "extern declarations without an explicit ABI are "
		     "deprecated");
  walk (block);
}

void
UnusedChecker::visit_loop_label (HIR::LoopLabel &label)
{
  auto lifetime = label.get_lifetime ();
  std::string var_name = lifetime.to_string ();
  auto id = lifetime.get_mappings ().get_hirid ();
  if (!unused_context.is_label_used (id) && var_name[0] != '_')
    rust_warning_at (lifetime.get_locus (), OPT_Wunused_variable,
		     "unused label %qs", lifetime.to_string ().c_str ());
}

void
UnusedChecker::visit (HIR::StructPatternFieldIdentPat &field)
{
  auto &pattern = field.get_pattern ();
  if (pattern.get_pattern_type () == HIR::Pattern::PatternType::IDENTIFIER)
    {
      auto &ident = static_cast<HIR::IdentifierPattern &> (pattern);
      if (!ident.has_subpattern ()
	  && ident.get_identifier ().as_string ()
	       == field.get_identifier ().as_string ())
	rust_warning_at (field.get_locus (), OPT_Wunused_variable,
			 "the %qs in this pattern is redundant",
			 (field.get_identifier ().as_string () + ":").c_str ());
    }
  walk (field);
}

namespace {

bool
literal_int_value (const HIR::Literal &lit, bool minus, int64_t &out)
{
  if (lit.get_lit_type () != HIR::Literal::LitType::INT)
    return false;

  std::string digits = lit.as_string ();
  digits.erase (std::remove (digits.begin (), digits.end (), '_'),
		digits.end ());

  char *end = nullptr;
  long long value = std::strtoll (digits.c_str (), &end, 10);
  if (end == digits.c_str () || *end != '\0')
    return false;

  out = minus ? -value : value;
  return true;
}

bool
range_bound_int (HIR::RangePatternBound &bound, int64_t &out)
{
  if (bound.get_bound_type ()
      != HIR::RangePatternBound::RangePatternBoundType::LITERAL)
    return false;

  auto &lit = static_cast<HIR::RangePatternBoundLiteral &> (bound);
  return literal_int_value (lit.get_literal (), lit.get_has_minus (), out);
}

} // namespace

void
UnusedChecker::visit (HIR::MatchExpr &expr)
{
  struct Range
  {
    int64_t lo;
    int64_t hi;
    bool inclusive;
    location_t locus;
  };
  std::vector<Range> ranges;
  std::vector<int64_t> starts;

  for (auto &match_case : expr.get_match_cases ())
    {
      auto &pattern = match_case.get_arm ().get_pattern ();
      if (!pattern)
	continue;

      if (pattern->get_pattern_type () == HIR::Pattern::PatternType::RANGE)
	{
	  auto &range = static_cast<HIR::RangePattern &> (*pattern);
	  int64_t lo, hi;
	  if (range_bound_int (range.get_lower_bound (), lo)
	      && range_bound_int (range.get_upper_bound (), hi))
	    {
	      ranges.push_back (
		{lo, hi, range.is_inclusive_range (), range.get_locus ()});
	      starts.push_back (lo);
	    }
	}
      else if (pattern->get_pattern_type ()
	       == HIR::Pattern::PatternType::LITERAL)
	{
	  auto &lit = static_cast<HIR::LiteralPattern &> (*pattern);
	  int64_t value;
	  if (literal_int_value (lit.get_literal (), lit.get_has_minus (),
				 value))
	    starts.push_back (value);
	}
    }

  // A value is covered if a range matches it or some arm starts exactly on it.
  auto covered = [&] (int64_t value) {
    if (std::find (starts.begin (), starts.end (), value) != starts.end ())
      return true;
    for (auto &range : ranges)
      {
	int64_t top = range.inclusive ? range.hi : range.hi - 1;
	if (value >= range.lo && value <= top)
	  return true;
      }
    return false;
  };

  // An exclusive range `lo..hi` leaves `hi` unmatched. If another arm picks up
  // at `hi + 1`, that single value was almost certainly meant to be included.
  for (auto &range : ranges)
    {
      if (range.inclusive)
	continue;

      int64_t missed = range.hi;
      if (!covered (missed)
	  && std::find (starts.begin (), starts.end (), missed + 1)
	       != starts.end ())
	rust_warning_at (range.locus, OPT_Wunused_variable,
			 "multiple ranges are one apart");
    }

  walk (expr);
}

void
UnusedChecker::visit (HIR::LetStmt &stmt)
{
  for (auto &attr : stmt.get_outer_attrs ())
    if (attr.get_path ().as_string () == Values::Attributes::DOC)
      {
	rust_warning_at (stmt.get_locus (), OPT_Wunused_variable,
			 "unused doc comment");
	break;
      }
  if (stmt.has_init_expr ()
      && stmt.get_init_expr ().get_expression_type ()
	   == HIR::Expr::ExprType::Block)
    {
      auto &block = static_cast<HIR::BlockExpr &> (stmt.get_init_expr ());
      if (block.get_statements ().empty () && block.has_expr ())
	rust_warning_at (block.get_locus (), OPT_Wunused,
			 "unnecessary braces around assigned value");
    }
  walk (stmt);
}

void
UnusedChecker::visit (HIR::BorrowExpr &expr)
{
  // The static_mut_refs lint: taking a reference to a mutable static is
  // discouraged as it can easily lead to undefined behaviour.
  NodeId ast_node_id = expr.get_expr ().get_mappings ().get_nodeid ();
  if (auto def
      = nr_context.lookup (ast_node_id, Resolver2_0::Namespace::Values))
    if (auto id = mappings.lookup_node_to_hir (*def))
      if (auto item = mappings.lookup_hir_item (*id))
	if (item.value ()->get_item_kind () == HIR::Item::ItemKind::Static)
	  {
	    auto &static_item = static_cast<HIR::StaticItem &> (*item.value ());
	    if (static_item.is_mut ())
	      rust_warning_at (expr.get_locus (), OPT_Wunused,
			       "creating a reference to a mutable static");
	  }
  walk (expr);
}

} // namespace Analysis
} // namespace Rust
