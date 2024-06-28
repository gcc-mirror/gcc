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
  if (!loan_errors.empty ())
    {
      rust_error_at (hir_function->get_locus (),
		     "Found loan errors in function %s",
		     hir_function->get_function_name ().as_string ().c_str ());
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

} // namespace BIR
} // namespace Rust
