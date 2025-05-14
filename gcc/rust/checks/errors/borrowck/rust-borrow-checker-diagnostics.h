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

#ifndef RUST_BORROW_CHECKER_DIAGNOSTICS_H
#define RUST_BORROW_CHECKER_DIAGNOSTICS_H

#include "polonius/rust-polonius.h"
#include "rust-bir.h"
#include "rust-hir-item.h"
#include "text-range-label.h"

namespace Rust {
namespace BIR {
class BorrowCheckerDiagnostics
{
  // HIR representation of Rust function
  const HIR::Function *hir_function;
  // BIR representation of Rust function
  const Function &bir_function;
  // Some facts related to this function
  const Polonius::Facts &facts;
  // Polonius output
  // Point - vector<Path>
  const std::vector<std::pair<size_t, std::vector<size_t>>> &move_errors;
  // Point - vector<Loan>
  const std::vector<std::pair<size_t, std::vector<size_t>>> &loan_errors;
  // Point - pair<Origin, Origin>
  const std::vector<std::pair<size_t, std::pair<size_t, size_t>>>
    &subset_errors;

public:
  BorrowCheckerDiagnostics (
    const HIR::Function *hir_function, const Function &bir_function,
    const Polonius::Facts &facts,
    const std::vector<std::pair<size_t, std::vector<size_t>>> &move_errors,
    const std::vector<std::pair<size_t, std::vector<size_t>>> &loan_errors,
    const std::vector<std::pair<size_t, std::pair<size_t, size_t>>>
      &subset_errors)

    : hir_function (hir_function), bir_function (bir_function), facts (facts),
      move_errors (move_errors), loan_errors (loan_errors),
      subset_errors (subset_errors)
  {}

  void report_errors ();

private:
  void report_move_errors ();
  void report_loan_errors ();
  void report_subset_errors ();

  const BIR::Statement &get_statement (Polonius::Point point);
  const BIR::Loan &get_loan (Polonius::Loan loan);
  const HIR::LifetimeParam *get_lifetime_param (Polonius::Origin origin);

  struct LabelLocationPair
  {
    text_range_label label;
    location_t location;
  };
  static void
  multi_label_error (const char *error_message, location_t error_location,
		     std::vector<LabelLocationPair> location_label_pairs);
};

} // namespace BIR
} // namespace Rust

#endif // RUST_BORROW_CHECKER_DIAGNOSTICS_H
