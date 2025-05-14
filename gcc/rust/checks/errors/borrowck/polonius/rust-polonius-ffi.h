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

#ifndef RUST_POLONIUS_FACTS_FFI_H
#define RUST_POLONIUS_FACTS_FFI_H

#include "rust-system.h"

// This file defines the C++ side of the FFI interface to Polonius.
// The corresponding Rust side is in `gccrs-ffi.rs`.

// IMPORTANT:
// This file intentionally does not include any C++ headers
// to allow seamless binding generation on the Rust side.

namespace Rust {
namespace Polonius {

using Origin = size_t;
using Loan = size_t;
/**
 * Compressed CFG point
 * Encoding:
 *  - (bit size - 16) bits: basic block index
 *  - 15 bits: stmt index inside basic block
 *  - 1bit: start or mid
 *
 * Note: Polonius requires the holding type to be `size_t`/`usize`.
 */
using Point = size_t;
using Variable = size_t;
using Path = size_t;

namespace FFI {

// NOTE: std::pair and std::tuple are complicating the bindings' generation.
template <typename T1, typename T2> struct Pair
{
  T1 first;
  T2 second;

  Pair (T1 first, T2 second) : first (first), second (second) {}
};

template <typename T1, typename T2, typename T3> struct Triple
{
  T1 first;
  T2 second;
  T3 third;

  Triple (T1 first, T2 second, T3 third)
    : first (first), second (second), third (third)
  {}
};

/** Frozen variant to vector for FFI */
template <typename T> struct Slice
{
  size_t len;
  const T *const data;

  template <typename vector>
  Slice (const vector &v) : len (v.size ()), data (v.data ())
  {}
};

/** Frozen variant of the facts with C ABI and no methods. */
struct FactsView
{
  Slice<Triple<Origin, Loan, Point>> loan_issued_at;
  Slice<Origin> universal_region;
  Slice<Pair<Point, Point>> cfg_edge;
  Slice<Pair<Loan, Point>> loan_killed_at;
  Slice<Triple<Origin, Origin, Point>> subset_base;
  Slice<Pair<Point, Loan>> loan_invalidated_at;
  Slice<Pair<Variable, Point>> var_used_at;
  Slice<Pair<Variable, Point>> var_defined_at;
  Slice<Pair<Variable, Point>> var_dropped_at;
  Slice<Pair<Variable, Origin>> use_of_var_derefs_origin;
  Slice<Pair<Variable, Origin>> drop_of_var_derefs_origin;
  Slice<Pair<Path, Path>> child_path;
  Slice<Pair<Path, Variable>> path_is_var;
  Slice<Pair<Path, Point>> path_assigned_at_base;
  Slice<Pair<Path, Point>> path_moved_at_base;
  Slice<Pair<Path, Point>> path_accessed_at_base;
  Slice<Pair<Origin, Origin>> known_placeholder_subset;
  Slice<Pair<Origin, Loan>> placeholder;
};

// Wrapper around std::vector to pass data from Rust to C++
template <typename T> struct FFIVector
{
  std::vector<T> data;

public:
  void push (T new_element) { data.push_back (new_element); };

  // allocates memory to a new instance and returns the pointer
  static FFIVector *make_new () { return new FFIVector{}; }

  // returns current size
  size_t size () const { return data.size (); }

  T at (size_t index) const
  {
    rust_assert (index < data.size ());
    return data.at (index);
  }
};

// Some useful type aliases
using FFIVectorPair = FFIVector<Pair<size_t, FFIVector<size_t> *>>;
using FFIVectorTriple = FFIVector<Triple<size_t, size_t, size_t>>;

inline std::vector<size_t>
make_vector (const FFIVector<size_t> *vec_sizet)
{
  std::vector<size_t> return_val (vec_sizet->size ());
  for (size_t i = 0; i < vec_sizet->size (); ++i)
    {
      return_val[i] = vec_sizet->at (i);
    }
  return return_val;
}

inline std::vector<std::pair<size_t, std::vector<size_t>>>
make_vector (const FFIVectorPair *vec_pair)
{
  std::vector<std::pair<size_t, std::vector<size_t>>> return_val (
    vec_pair->size ());
  for (size_t i = 0; i < vec_pair->size (); ++i)
    {
      std::pair<size_t, std::vector<size_t>> current_pair
	= {vec_pair->at (i).first, make_vector (vec_pair->at (i).second)};
      return_val[i] = current_pair;
    }
  return return_val;
}

inline std::vector<std::pair<size_t, std::pair<size_t, size_t>>>
make_vector (const FFIVectorTriple *vec_triple)
{
  std::vector<std::pair<size_t, std::pair<size_t, size_t>>> return_val (
    vec_triple->size ());
  for (size_t i = 0; i < vec_triple->size (); ++i)
    {
      auto current_element = std::pair<size_t, std::pair<size_t, size_t>>{
	vec_triple->at (i).first,
	{vec_triple->at (i).second, vec_triple->at (i).third}};
      return_val[i] = current_element;
    }
  return return_val;
}

struct Output
{
  FFIVectorPair *loan_errors;
  FFIVectorPair *move_errors;
  FFIVectorTriple *subset_errors;
};

} // namespace FFI
} // namespace Polonius
} // namespace Rust

#endif // RUST_POLONIUS_FACTS_FFI_H
