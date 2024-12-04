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

#include "rust-ast-builder-type.h"
#include "rust-ast-builder.h"
#include "rust-ast-full.h"
#include "rust-common.h"

namespace Rust {
namespace AST {

ASTTypeBuilder::ASTTypeBuilder () : translated (nullptr) {}

Type *
ASTTypeBuilder::build (Type &type)
{
  ASTTypeBuilder builder;
  type.accept_vis (builder);
  rust_assert (builder.translated != nullptr);
  return builder.translated;
}

void
ASTTypeBuilder::visit (BareFunctionType &fntype)
{
  /* TODO */
}

void
ASTTypeBuilder::visit (TupleType &tuple)
{
  std::vector<std::unique_ptr<Type> > elems;
  for (auto &elem : tuple.get_elems ())
    {
      Type *t = ASTTypeBuilder::build (*elem.get ());
      std::unique_ptr<Type> ty (t);
      elems.push_back (std::move (ty));
    }
  translated = new TupleType (std::move (elems), tuple.get_locus ());
}

void
ASTTypeBuilder::visit (TypePath &path)
{
  std::vector<std::unique_ptr<TypePathSegment> > segments;
  for (auto &seg : path.get_segments ())
    {
      switch (seg->get_type ())
	{
	  case TypePathSegment::REG: {
	    const TypePathSegment &segment
	      = (const TypePathSegment &) (*seg.get ());
	    TypePathSegment *s
	      = new TypePathSegment (segment.get_ident_segment (),
				     segment.get_separating_scope_resolution (),
				     segment.get_locus ());
	    std::unique_ptr<TypePathSegment> sg (s);
	    segments.push_back (std::move (sg));
	  }
	  break;

	  case TypePathSegment::GENERIC: {
	    TypePathSegmentGeneric &generic
	      = (TypePathSegmentGeneric &) (*seg.get ());

	    GenericArgs args
	      = Builder::new_generic_args (generic.get_generic_args ());
	    TypePathSegmentGeneric *s
	      = new TypePathSegmentGeneric (generic.get_ident_segment (), false,
					    std::move (args),
					    generic.get_locus ());
	    std::unique_ptr<TypePathSegment> sg (s);
	    segments.push_back (std::move (sg));
	  }
	  break;

	  case TypePathSegment::FUNCTION: {
	    rust_unreachable ();
	    // TODO
	    // const TypePathSegmentFunction &fn
	    //   = (const TypePathSegmentFunction &) (*seg.get ());
	  }
	  break;
	}
    }

  translated = new TypePath (std::move (segments), path.get_locus (),
			     path.has_opening_scope_resolution_op ());
}

void
ASTTypeBuilder::visit (QualifiedPathInType &path)
{
  /* TODO */
}

void
ASTTypeBuilder::visit (ArrayType &type)
{
  /* TODO */
}

void
ASTTypeBuilder::visit (ReferenceType &type)
{
  /* TODO */
}

void
ASTTypeBuilder::visit (RawPointerType &type)
{
  /* TODO */
}

void
ASTTypeBuilder::visit (SliceType &type)
{
  Type *t = ASTTypeBuilder::build (type.get_elem_type ());
  std::unique_ptr<Type> ty (t);
  translated = new SliceType (std::move (ty), type.get_locus ());
}

void
ASTTypeBuilder::visit (InferredType &type)
{
  translated = new InferredType (type.get_locus ());
}

void
ASTTypeBuilder::visit (NeverType &type)
{
  translated = new NeverType (type.get_locus ());
}

void
ASTTypeBuilder::visit (TraitObjectTypeOneBound &type)
{
  /* TODO */
}

void
ASTTypeBuilder::visit (TraitObjectType &type)
{
  /* TODO */
}

} // namespace AST
} // namespace Rust
