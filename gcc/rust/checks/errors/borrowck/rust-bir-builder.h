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

#ifndef RUST_BIR_BUILDER_H
#define RUST_BIR_BUILDER_H

#include "rust-bir-builder-internal.h"
#include "rust-hir-visitor.h"
#include "rust-bir-builder-pattern.h"
#include "rust-bir-builder-struct.h"
#include "rust-bir-builder-expr-stmt.h"

namespace Rust {
namespace BIR {

/** Top-level builder, which compiles a HIR function into a BIR function. */
class Builder : public AbstractBuilder
{
public:
  explicit Builder (BuilderContext &ctx) : AbstractBuilder (ctx) {}

  Function build (HIR::Function &function)
  {
    PlaceId return_place
      = ctx.place_db.add_temporary (lookup_type (*function.get_definition ()));
    rust_assert (return_place == RETURN_VALUE_PLACE);

    for (auto &param : function.get_function_params ())
      {
	handle_param (param);
      }

    handle_body (*function.get_definition ());

    return Function{std::move (ctx.place_db), std::move (ctx.arguments),
		    std::move (ctx.basic_blocks)};
  };

private:
  void handle_param (HIR::FunctionParam &param)
  {
    auto &pattern = param.get_param_name ();
    if (pattern->get_pattern_type () == HIR::Pattern::IDENTIFIER
	&& !static_cast<HIR::IdentifierPattern &> (*pattern).get_is_ref ())
      {
	// Avoid useless temporary variable for parameter.
	translated = declare_variable (pattern->get_mappings ());
	ctx.arguments.push_back (translated);
      }
    else
      {
	translated = ctx.place_db.add_temporary (lookup_type (*pattern));
	ctx.arguments.push_back (translated);
	PatternBindingBuilder (ctx, translated, param.get_type ().get ())
	  .go (*param.get_param_name ());
      }
  }

  void handle_body (HIR::BlockExpr &body)
  {
    translated = ExprStmtBuilder (ctx).build (body);
    if (body.has_expr () && !lookup_type (body)->is_unit ())
      {
	push_assignment (RETURN_VALUE_PLACE, translated);
	ctx.get_current_bb ().statements.emplace_back (Node::Kind::RETURN);
      }
    else if (!ctx.get_current_bb ().is_terminated ())
      {
	push_assignment (RETURN_VALUE_PLACE,
			 ctx.place_db.get_constant (lookup_type (body)));
	ctx.get_current_bb ().statements.emplace_back (Node::Kind::RETURN);
      }
  };
};

} // namespace BIR
} // namespace Rust

#endif // RUST_BIR_BUILDER_H
