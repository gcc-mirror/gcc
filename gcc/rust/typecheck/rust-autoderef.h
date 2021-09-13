// Copyright (C) 2020 Free Software Foundation, Inc.

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

#ifndef RUST_AUTODEREF
#define RUST_AUTODEREF

#include "rust-tyty.h"

namespace Rust {
namespace Resolver {

class Adjustment
{
public:
  enum AdjustmentType
  {
    IMM_REF,
    MUT_REF,
    DEREF_REF
  };

  Adjustment (AdjustmentType type, TyTy::BaseType *expected)
    : type (type), expected (expected)
  {}

  AdjustmentType get_type () const { return type; }

  TyTy::BaseType *get_expected () const { return expected; }

  std::string as_string () const
  {
    return Adjustment::type_string (get_type ()) + "->"
	   + get_expected ()->debug_str ();
  }

  static std::string type_string (AdjustmentType type)
  {
    switch (type)
      {
      case AdjustmentType::IMM_REF:
	return "IMM_REF";
      case AdjustmentType::MUT_REF:
	return "MUT_REF";
      case AdjustmentType::DEREF_REF:
	return "DEREF_REF";
      }
    gcc_unreachable ();
    return "";
  }

private:
  AdjustmentType type;
  TyTy::BaseType *expected;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AUTODEREF
