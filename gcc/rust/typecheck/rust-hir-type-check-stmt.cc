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

#include "rust-hir-type-check-stmt.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-expr.h"
#include "rust-hir-type-check-implitem.h"
#include "rust-hir-type-check-item.h"
#include "rust-hir-type-check-pattern.h"
#include "rust-type-util.h"

namespace Rust {
namespace Resolver {

TyTy::BaseType *
TypeCheckStmt::Resolve (HIR::Stmt *stmt)
{
  TypeCheckStmt resolver;
  stmt->accept_vis (resolver);
  return resolver.infered;
}

void
TypeCheckStmt::visit (HIR::ExprStmt &stmt)
{
  infered = TypeCheckExpr::Resolve (stmt.get_expr ().get ());
}

void
TypeCheckStmt::visit (HIR::EmptyStmt &stmt)
{
  infered = TyTy::TupleType::get_unit_type (stmt.get_mappings ().get_hirid ());
}

void
TypeCheckStmt::visit (HIR::ExternBlock &extern_block)
{
  for (auto &item : extern_block.get_extern_items ())
    {
      TypeCheckTopLevelExternItem::Resolve (item.get (), extern_block);
    }
}

void
TypeCheckStmt::visit (HIR::ConstantItem &constant)
{
  TyTy::BaseType *type = TypeCheckType::Resolve (constant.get_type ().get ());
  TyTy::BaseType *expr_type
    = TypeCheckExpr::Resolve (constant.get_expr ().get ());

  infered = coercion_site (
    constant.get_mappings ().get_hirid (),
    TyTy::TyWithLocation (type, constant.get_type ()->get_locus ()),
    TyTy::TyWithLocation (expr_type, constant.get_expr ()->get_locus ()),
    constant.get_locus ());
  context->insert_type (constant.get_mappings (), infered);
}

void
TypeCheckStmt::visit (HIR::LetStmt &stmt)
{
  infered = TyTy::TupleType::get_unit_type (stmt.get_mappings ().get_hirid ());

  HIR::Pattern &stmt_pattern = *stmt.get_pattern ();
  TyTy::BaseType *init_expr_ty = nullptr;
  location_t init_expr_locus = UNKNOWN_LOCATION;
  if (stmt.has_init_expr ())
    {
      init_expr_locus = stmt.get_init_expr ()->get_locus ();
      init_expr_ty = TypeCheckExpr::Resolve (stmt.get_init_expr ().get ());
      if (init_expr_ty->get_kind () == TyTy::TypeKind::ERROR)
	return;

      init_expr_ty->append_reference (
	stmt_pattern.get_mappings ().get_hirid ());
    }

  TyTy::BaseType *specified_ty = nullptr;
  location_t specified_ty_locus;
  if (stmt.has_type ())
    {
      specified_ty = TypeCheckType::Resolve (stmt.get_type ().get ());
      specified_ty_locus = stmt.get_type ()->get_locus ();
    }

  // let x:i32 = 123;
  if (specified_ty != nullptr && init_expr_ty != nullptr)
    {
      coercion_site (stmt.get_mappings ().get_hirid (),
		     TyTy::TyWithLocation (specified_ty, specified_ty_locus),
		     TyTy::TyWithLocation (init_expr_ty, init_expr_locus),
		     stmt.get_locus ());
      TypeCheckPattern::Resolve (&stmt_pattern, specified_ty);
    }
  else
    {
      // let x:i32;
      if (specified_ty != nullptr)
	{
	  TypeCheckPattern::Resolve (&stmt_pattern, specified_ty);
	}
      // let x = 123;
      else if (init_expr_ty != nullptr)
	{
	  TypeCheckPattern::Resolve (&stmt_pattern, init_expr_ty);
	}
      // let x;
      else
	{
	  auto infer
	    = new TyTy::InferType (stmt_pattern.get_mappings ().get_hirid (),
				   TyTy::InferType::InferTypeKind::GENERAL,
				   TyTy::InferType::TypeHint::Default (),
				   stmt.get_locus ());
	  TypeCheckPattern::Resolve (&stmt_pattern, infer);
	}
    }
}

void
TypeCheckStmt::visit (HIR::TypePath &path)
{
  infered = TypeCheckType::Resolve (&path);
}
void
TypeCheckStmt::visit (HIR::QualifiedPathInType &path)
{
  infered = TypeCheckType::Resolve (&path);
}

void
TypeCheckStmt::visit (HIR::TupleStruct &struct_decl)
{
  infered = TypeCheckItem::Resolve (struct_decl);
}

void
TypeCheckStmt::visit (HIR::Enum &enum_decl)
{
  infered = TypeCheckItem::Resolve (enum_decl);
}

void
TypeCheckStmt::visit (HIR::StructStruct &struct_decl)
{
  infered = TypeCheckItem::Resolve (struct_decl);
}

void
TypeCheckStmt::visit (HIR::Union &union_decl)
{
  infered = TypeCheckItem::Resolve (union_decl);
}

void
TypeCheckStmt::visit (HIR::Function &function)
{
  infered = TypeCheckItem::Resolve (function);
}

void
TypeCheckStmt::visit (HIR::Module &module)
{
  infered = TypeCheckItem::Resolve (module);
}

void
TypeCheckStmt::visit (HIR::TypeAlias &type_alias)
{
  infered = TypeCheckItem::Resolve (type_alias);
}

void
TypeCheckStmt::visit (HIR::StaticItem &static_item)
{
  infered = TypeCheckItem::Resolve (static_item);
}

void
TypeCheckStmt::visit (HIR::Trait &trait)
{
  infered = TypeCheckItem::Resolve (trait);
}

void
TypeCheckStmt::visit (HIR::ImplBlock &impl)
{
  infered = TypeCheckItem::Resolve (impl);
}

} // namespace Resolver
} // namespace Rust
