// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#include "rust-borrow-checker-diagnostics.h"
#include "polonius/rust-polonius-ffi.h"
#include "rust-diagnostics.h"

namespace Rust {
namespace BIR {

void
BorrowCheckerDiagnostics::report_errors ()
{
  report_move_errors ();
  report_loan_errors ();
  report_subset_errors ();
}

void
BorrowCheckerDiagnostics::report_move_errors ()
{
  for (const auto &pair : move_errors)
    {
      auto error_location = get_statement (pair.first).get_location ();

      // in future, we can use the assigned at location to hint the
      // user to implement copy trait for the type
      /*
      for (auto it : facts.path_assigned_at_base)
	{
	  if (pair.second[0] == it.first)
	    {
	      auto point_assigned_at = it.second;
	      auto assigned_at_location
		= get_statement (point_assigned_at).get_location ();
	    }
	}
	*/

      std::vector<LabelLocationPair> labels{
	{"moved value used here", error_location}};
      // add labels to all the moves for the given path
      for (auto it : facts.path_moved_at_base)
	{
	  if (pair.second[0] == it.first)
	    {
	      auto point_moved_at = it.second;
	      // don't label the move location where the error occured
	      if (pair.first != point_moved_at)
		{
		  auto move_at_location
		    = get_statement (point_moved_at).get_location ();
		  labels.push_back ({"value moved here", move_at_location});
		}
	    }
	}
      multi_label_error ("use of moved value", error_location, labels);
    }
}

void
BorrowCheckerDiagnostics::report_loan_errors ()
{
  for (const auto &pair : loan_errors)
    {
      auto error_location = get_statement (pair.first).get_location ();
      for (const auto &loan : pair.second)
	{
	  auto loan_struct = get_loan (loan);
	  multi_label_error ("use of borrowed value", error_location,
			     {{"borrow occurs here", loan_struct.location},
			      {"borrowed value used here", error_location}});
	}
    }
}

void
BorrowCheckerDiagnostics::report_subset_errors ()
{
  // remove duplicates in subset_errors
  //
  // Polonius may output subset errors for same 2 origins at multiple points
  // so to avoid duplicating the errors, we can remove the elements in subset
  // errors with same origin pair
  std::vector<std::pair<size_t, std::pair<size_t, size_t>>>
    deduplicated_subset_errors;

  for (auto pair : subset_errors)
    {
      auto it = std::find_if (
	deduplicated_subset_errors.begin (), deduplicated_subset_errors.end (),
	[&pair] (std::pair<size_t, std::pair<size_t, size_t>> element) {
	  return element.second == pair.second;
	});
      if (it == deduplicated_subset_errors.end ())
	{
	  deduplicated_subset_errors.push_back (pair);
	}
    }
  for (const auto &error : deduplicated_subset_errors)
    {
      auto first_lifetime_location
	= get_lifetime_param (error.second.first)->get_locus ();
      auto second_lifetime_location
	= get_lifetime_param (error.second.second)->get_locus ();
      multi_label_error (
	"subset error, some lifetime constraints need to be added",
	bir_function.location,
	{{"lifetime defined here", first_lifetime_location},
	 {"lifetime defined here", second_lifetime_location},
	 {"subset error occurs in this function", bir_function.location}});
    }
}

const BIR::Statement &
BorrowCheckerDiagnostics::get_statement (Polonius::Point point)
{
  auto statement_index = Polonius::FullPoint::extract_stmt (point);
  auto bb_index = Polonius::FullPoint::extract_bb (point);
  // assert that the extracted indexes are valid
  rust_assert (bb_index < bir_function.basic_blocks.size ());
  rust_assert (statement_index
	       < bir_function.basic_blocks[{bb_index}].statements.size ());
  return bir_function.basic_blocks[{bb_index}].statements[statement_index];
}

const BIR::Loan &
BorrowCheckerDiagnostics::get_loan (Polonius::Loan loan)
{
  return bir_function.place_db.get_loans ()[{(uint32_t) loan}];
}

const HIR::LifetimeParam *
BorrowCheckerDiagnostics::get_lifetime_param (Polonius::Origin origin)

{
  return bir_function.region_hir_map.at (origin);
}

void
BorrowCheckerDiagnostics::multi_label_error (
  const char *error_message, location_t error_location,
  std::vector<LabelLocationPair> location_label_pairs)
{
  rich_location r{line_table, error_location};
  for (auto &label_location : location_label_pairs)
    {
      r.add_range (label_location.location, SHOW_RANGE_WITHOUT_CARET,
		   &label_location.label);
    }
  rust_error_at (r, "%s", error_message);
}

} // namespace BIR
} // namespace Rust
