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
  if (!move_errors.empty ())
    {
      rust_error_at (hir_function->get_locus (),
		     "Found move errors in function %s",
		     hir_function->get_function_name ().as_string ().c_str ());
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
  if (!subset_errors.empty ())
    {
      rust_error_at (hir_function->get_locus (),
		     "Found subset errors in function %s. Some lifetime "
		     "constraints need to be added.",
		     hir_function->get_function_name ().as_string ().c_str ());
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
	       < bir_function.basic_blocks[bb_index].statements.size ());
  return bir_function.basic_blocks[bb_index].statements[statement_index];
}

const BIR::Loan &
BorrowCheckerDiagnostics::get_loan (Polonius::Loan loan)
{
  return bir_function.place_db.get_loans ()[loan];
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
