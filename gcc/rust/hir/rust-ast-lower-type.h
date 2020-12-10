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

#ifndef RUST_AST_LOWER_TYPE
#define RUST_AST_LOWER_TYPE

#include "rust-ast-lower-base.h"
#include "rust-diagnostics.h"

namespace Rust {
namespace HIR {

class ASTLoweringType : public ASTLoweringBase
{
public:
  static HIR::Type *translate (AST::Type *type)
  {
    ASTLoweringType resolver;
    type->accept_vis (resolver);
    return resolver.translated;
  }

  virtual ~ASTLoweringType () {}

  virtual void visit (AST::TypePathSegment &segment)
  {
    HIR::PathIdentSegment ident (segment.get_ident_segment ().as_string ());
    translated_segment
      = new HIR::TypePathSegment (ident,
				  segment.get_separating_scope_resolution (),
				  segment.get_locus ());
  }

  virtual void visit (AST::TypePath &path)
  {
    std::vector<std::unique_ptr<HIR::TypePathSegment> > translated_segments;

    path.iterate_segments ([&] (AST::TypePathSegment *seg) mutable -> bool {
      translated_segment = nullptr;
      seg->accept_vis (*this);
      if (translated_segment == nullptr)
	{
	  rust_fatal_error (seg->get_locus (),
			    "failed to translate AST TypePathSegment");
	  return false;
	}

      translated_segments.push_back (
	std::unique_ptr<HIR::TypePathSegment> (translated_segment));
      return true;
    });

    translated
      = new HIR::TypePath (std::move (translated_segments), path.get_locus (),
			   path.has_opening_scope_resolution_op ());
  }

private:
  ASTLoweringType () : translated (nullptr) {}

  HIR::Type *translated;

  HIR::TypePathSegment *translated_segment;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_TYPE
