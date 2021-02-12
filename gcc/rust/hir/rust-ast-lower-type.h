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
#include "rust-ast-lower-expr.h"

namespace Rust {
namespace HIR {

class ASTLoweringType : public ASTLoweringBase
{
public:
  static HIR::Type *translate (AST::Type *type)
  {
    ASTLoweringType resolver;
    type->accept_vis (resolver);

    rust_assert (resolver.translated != nullptr);
    resolver.mappings->insert_location (
      resolver.translated->get_mappings ().get_crate_num (),
      resolver.translated->get_mappings ().get_hirid (),
      type->get_locus_slow ());

    return resolver.translated;
  }

  void visit (AST::BareFunctionType &fntype)
  {
    bool is_variadic = false;
    std::vector<HIR::LifetimeParam> lifetime_params;
    HIR::FunctionQualifiers qualifiers (
      HIR::FunctionQualifiers::AsyncConstStatus::NONE, false);

    std::vector<HIR::MaybeNamedParam> named_params;
    for (auto &param : fntype.get_function_params ())
      {
	HIR::MaybeNamedParam::ParamKind kind;
	switch (param.get_param_kind ())
	  {
	  case AST::MaybeNamedParam::ParamKind::UNNAMED:
	    kind = HIR::MaybeNamedParam::ParamKind::UNNAMED;
	    break;
	  case AST::MaybeNamedParam::ParamKind::IDENTIFIER:
	    kind = HIR::MaybeNamedParam::ParamKind::IDENTIFIER;
	    break;
	  case AST::MaybeNamedParam::ParamKind::WILDCARD:
	    kind = HIR::MaybeNamedParam::ParamKind::WILDCARD;
	    break;
	  }

	HIR::Type *param_type
	  = ASTLoweringType::translate (param.get_type ().get ());

	HIR::MaybeNamedParam p (param.get_name (), kind,
				std::unique_ptr<HIR::Type> (param_type),
				param.get_locus ());
	named_params.push_back (std::move (p));
      }

    HIR::Type *return_type = nullptr;
    if (fntype.has_return_type ())
      {
	return_type
	  = ASTLoweringType::translate (fntype.get_return_type ().get ());
      }

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, fntype.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated = new HIR::BareFunctionType (
      std::move (mapping), std::move (lifetime_params), std::move (qualifiers),
      std::move (named_params), is_variadic,
      std::unique_ptr<HIR::Type> (return_type), fntype.get_locus ());
  }

  void visit (AST::TupleType &tuple)
  {
    std::vector<std::unique_ptr<HIR::Type> > elems;
    for (auto &e : tuple.get_elems ())
      {
	HIR::Type *t = ASTLoweringType::translate (e.get ());
	elems.push_back (std::unique_ptr<HIR::Type> (t));
      }

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, tuple.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated = new HIR::TupleType (std::move (mapping), std::move (elems),
				     tuple.get_locus ());
  }

  void visit (AST::TypePathSegment &segment)
  {
    HIR::PathIdentSegment ident (segment.get_ident_segment ().as_string ());
    translated_segment
      = new HIR::TypePathSegment (ident,
				  segment.get_separating_scope_resolution (),
				  segment.get_locus ());
  }

  void visit (AST::TypePath &path)
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

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, path.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));
    translated
      = new HIR::TypePath (std::move (mapping), std::move (translated_segments),
			   path.get_locus (),
			   path.has_opening_scope_resolution_op ());
    mappings->insert_hir_type (mapping.get_crate_num (), mapping.get_hirid (),
			       translated);
  }

  void visit (AST::ArrayType &type)
  {
    HIR::Type *translated_type
      = ASTLoweringType::translate (type.get_elem_type ().get ());
    HIR::Expr *array_size
      = ASTLoweringExpr::translate (type.get_size_expr ().get ());

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated
      = new HIR::ArrayType (mapping,
			    std::unique_ptr<HIR::Type> (translated_type),
			    std::unique_ptr<HIR::Expr> (array_size),
			    type.get_locus ());
    mappings->insert_hir_type (mapping.get_crate_num (), mapping.get_hirid (),
			       translated);
  }

  void visit (AST::ReferenceType &type)
  {
    HIR::Lifetime lifetime = lower_lifetime (type.get_lifetime ());

    HIR::Type *base_type
      = ASTLoweringType::translate (type.get_base_type ().get ());

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated = new HIR::ReferenceType (mapping, type.get_has_mut (),
					 std::unique_ptr<HIR::Type> (base_type),
					 type.get_locus (), lifetime);

    mappings->insert_hir_type (mapping.get_crate_num (), mapping.get_hirid (),
			       translated);
  }

  void visit (AST::InferredType &type)
  {
    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated = new HIR::InferredType (mapping, type.get_locus ());

    mappings->insert_hir_type (mapping.get_crate_num (), mapping.get_hirid (),
			       translated);
  }

private:
  ASTLoweringType ()
    : ASTLoweringBase (), translated (nullptr), translated_segment (nullptr)
  {}

  HIR::Type *translated;
  HIR::TypePathSegment *translated_segment;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_TYPE
