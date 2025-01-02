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

#include "rust-compile-struct-field-expr.h"
#include "rust-compile-expr.h"

namespace Rust {
namespace Compile {

CompileStructExprField::CompileStructExprField (Context *ctx)
  : HIRCompileBase (ctx), translated (error_mark_node)
{}

tree
CompileStructExprField::Compile (HIR::StructExprField *field, Context *ctx)
{
  CompileStructExprField compiler (ctx);
  switch (field->get_kind ())
    {
    case HIR::StructExprField::StructExprFieldKind::IDENTIFIER:
      compiler.visit (static_cast<HIR::StructExprFieldIdentifier &> (*field));
      break;

    case HIR::StructExprField::StructExprFieldKind::IDENTIFIER_VALUE:
      compiler.visit (
	static_cast<HIR::StructExprFieldIdentifierValue &> (*field));
      break;

    case HIR::StructExprField::StructExprFieldKind::INDEX_VALUE:
      compiler.visit (static_cast<HIR::StructExprFieldIndexValue &> (*field));
      break;
    }
  return compiler.translated;
}

void
CompileStructExprField::visit (HIR::StructExprFieldIdentifierValue &field)
{
  translated = CompileExpr::Compile (field.get_value ().get (), ctx);
}

void
CompileStructExprField::visit (HIR::StructExprFieldIndexValue &field)
{
  translated = CompileExpr::Compile (field.get_value ().get (), ctx);
}

void
CompileStructExprField::visit (HIR::StructExprFieldIdentifier &field)
{
  // we can make the field look like a path expr to take advantage of existing
  // code

  Analysis::NodeMapping mappings_copy1 = field.get_mappings ();
  Analysis::NodeMapping mappings_copy2 = field.get_mappings ();

  HIR::PathIdentSegment ident_seg (field.get_field_name ().as_string ());
  HIR::PathExprSegment seg (mappings_copy1, ident_seg, field.get_locus (),
			    HIR::GenericArgs::create_empty ());
  HIR::PathInExpression expr (mappings_copy2, {seg}, field.get_locus (), false,
			      {});
  translated = CompileExpr::Compile (&expr, ctx);
}

} // namespace Compile
} // namespace Rust
