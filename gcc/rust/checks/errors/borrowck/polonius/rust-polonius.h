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

#ifndef RUST_POLONIUS_H
#define RUST_POLONIUS_H

// Interface to the Polonius borrow checker engine.
// See (https://github.com/rust-lang/polonius/blob/master/polonius-engine/)

#include <ostream>
#include "rust-polonius-ffi.h"

namespace Rust {
namespace Polonius {

/** A point in the control flow graph. */
struct FullPoint
{
  uint32_t bb;
  uint32_t stmt;
  bool mid;

  /** Expands a compressed `Point` into its components.
   * See `Point` docs for encoding details.
   */
  explicit FullPoint (Point point)
    : bb (extract_bb (point)), stmt (extract_stmt (point)),
      mid (extract_mid (point))
  {}

  static uint32_t extract_bb (Point point) { return point >> 16; }
  static uint32_t extract_stmt (Point point)
  {
    return (point & ~(1 << 16)) >> 1;
  }
  static bool extract_mid (Point point) { return point & 1; }

  friend std::ostream &operator<< (std::ostream &os, const FullPoint &point)
  {
    return os << ((point.mid) ? "Mid" : "Start") << "(bb" << point.bb << "["
	      << point.stmt << "])";
  }
};

struct Facts
{
  // See (https://rust-lang.github.io/polonius/rules/relations.html)
  std::vector<FFI::Triple<Origin, Loan, Point>> loan_issued_at;
  std::vector<Origin> universal_region;
  std::vector<FFI::Pair<Point, Point>> cfg_edge;
  std::vector<FFI::Pair<Loan, Point>> loan_killed_at;
  std::vector<FFI::Triple<Origin, Origin, Point>> subset_base;
  std::vector<FFI::Pair<Point, Loan>> loan_invalidated_at;
  std::vector<FFI::Pair<Variable, Point>> var_used_at;
  std::vector<FFI::Pair<Variable, Point>> var_defined_at;
  std::vector<FFI::Pair<Variable, Point>> var_dropped_at;
  std::vector<FFI::Pair<Variable, Origin>> use_of_var_derefs_origin;
  std::vector<FFI::Pair<Variable, Origin>> drop_of_var_derefs_origin;
  std::vector<FFI::Pair<Path, Path>> child_path;
  std::vector<FFI::Pair<Path, Variable>> path_is_var;
  std::vector<FFI::Pair<Path, Point>> path_assigned_at_base;
  std::vector<FFI::Pair<Path, Point>> path_moved_at_base;
  std::vector<FFI::Pair<Path, Point>> path_accessed_at_base;
  std::vector<FFI::Pair<Origin, Origin>> known_placeholder_subset;
  std::vector<FFI::Pair<Origin, Loan>> placeholder;

  /**
   * Create a const view for the struct for FFI.
   *
   * This view uses the original vector storage.
   * Therefore any resizing operation of Facts member may invalidate the view.
   */
  FFI::FactsView freeze ()
  {
    return FFI::FactsView{loan_issued_at,
			  universal_region,
			  cfg_edge,
			  loan_killed_at,
			  subset_base,
			  loan_invalidated_at,
			  var_used_at,
			  var_defined_at,
			  var_dropped_at,
			  use_of_var_derefs_origin,
			  drop_of_var_derefs_origin,
			  child_path,
			  path_is_var,
			  path_assigned_at_base,
			  path_moved_at_base,
			  path_accessed_at_base,
			  known_placeholder_subset,
			  placeholder};
  }

  void dump_loan_issued_at (std::ostream &os) const
  {
    for (auto &e : loan_issued_at)
      os << "\"'?" << e.first << "\"\t\"bw" << e.second << "\"\t\""
	 << FullPoint (e.third) << "\"\n";
  }

  void dump_universal_region (std::ostream &os) const
  {
    for (auto &e : universal_region)
      os << e << "\n";
  }

  void dump_cfg_edge (std::ostream &os) const
  {
    for (auto &e : cfg_edge)
      os << FullPoint (e.first) << " " << FullPoint (e.second) << "\n";
  }

  void dump_loan_killed_at (std::ostream &os) const
  {
    for (auto &e : loan_killed_at)
      os << e.first << " " << FullPoint (e.second) << "\n";
  }

  void dump_subset_base (std::ostream &os) const
  {
    for (auto &e : subset_base)
      os << "\"'?" << e.first << "\"\t\"'?" << e.second << "\"\t\""
	 << FullPoint (e.third) << "\"\n";
  }

  void dump_loan_invalidated_at (std::ostream &os) const
  {
    for (auto &e : loan_invalidated_at)
      os << FullPoint (e.first) << " " << e.second << "\n";
  }

  void dump_var_used_at (std::ostream &os) const
  {
    for (auto &e : var_used_at)
      os << e.first - 1 << " " << FullPoint (e.second) << "\n";
  }

  void dump_var_defined_at (std::ostream &os) const
  {
    for (auto &e : var_defined_at)
      os << e.first << " " << FullPoint (e.second) << "\n";
  }

  void dump_var_dropped_at (std::ostream &os) const
  {
    for (auto &e : var_dropped_at)
      os << e.first << " " << FullPoint (e.second) << "\n";
  }

  void dump_use_of_var_derefs_origin (std::ostream &os) const
  {
    for (auto &e : use_of_var_derefs_origin)
      os << e.first << " " << e.second << "\n";
  }

  void dump_drop_of_var_derefs_origin (std::ostream &os) const
  {
    for (auto &e : drop_of_var_derefs_origin)
      os << e.first << " " << e.second << "\n";
  }

  void dump_child_path (std::ostream &os) const
  {
    for (auto &e : child_path)
      os << e.first << " " << e.second << "\n";
  }

  void dump_path_is_var (std::ostream &os) const
  {
    for (auto &e : path_is_var)
      os << e.first << " " << e.second << "\n";
  }

  void dump_path_assigned_at_base (std::ostream &os) const
  {
    for (auto &e : path_assigned_at_base)
      os << e.first << " " << FullPoint (e.second) << "\n";
  }

  void dump_path_moved_at_base (std::ostream &os) const
  {
    for (auto &e : path_moved_at_base)
      os << e.first << " " << FullPoint (e.second) << "\n";
  }

  void dump_path_accessed_at_base (std::ostream &os) const
  {
    for (auto &e : path_accessed_at_base)
      os << e.first << " " << FullPoint (e.second) << "\n";
  }

  void dump_known_placeholder_subset (std::ostream &os) const
  {
    for (auto &e : known_placeholder_subset)
      os << e.first << " " << e.second << "\n";
  }

  void dump_placeholder (std::ostream &os) const
  {
    for (auto &e : placeholder)
      os << e.first << " " << e.second << "\n";
  }
};

/**
 * Check a single function for borrow errors.
 *
 * Output is not yet implemented and is only dumped to stdout.
 */
extern "C" void
polonius_run (FFI::FactsView input, bool dump_enabled);

} // namespace Polonius
} // namespace Rust

#endif /* !RUST_POLONIUS_H */
