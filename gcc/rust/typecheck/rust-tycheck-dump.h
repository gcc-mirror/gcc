// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#ifndef RUST_TYCHECK_DUMP
#define RUST_TYCHECK_DUMP

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"

namespace Rust {
namespace Resolver {

class TypeResolverDump : private TypeCheckBase, private HIR::HIRFullVisitorBase
{
  using HIR::HIRFullVisitorBase::visit;

public:
  static void go (HIR::Crate &crate, std::ofstream &out)
  {
    TypeResolverDump dumper;
    for (auto &item : crate.items)
      {
	item->accept_vis (dumper);
	dumper.dump += "\n";
      }

    out << dumper.dump;
  }

  void visit (HIR::StructStruct &struct_decl) override
  {
    dump += indent () + "struct " + type_string (struct_decl.get_mappings ())
	    + "\n";
  }

  void visit (HIR::Union &union_decl) override
  {
    dump
      += indent () + "union " + type_string (union_decl.get_mappings ()) + "\n";
  }

  void visit (HIR::TupleStruct &struct_decl) override
  {
    dump += indent () + "struct" + type_string (struct_decl.get_mappings ())
	    + "\n";
  }

  void visit (HIR::ImplBlock &impl_block) override
  {
    dump += indent () + "impl "
	    + type_string (impl_block.get_type ()->get_mappings ()) + " {\n";
    indentation_level++;

    for (auto &impl_item : impl_block.get_impl_items ())
      {
	impl_item->accept_vis (*this);
	dump += "\n";
      }

    indentation_level--;
    dump += indent () + "}\n";
  }

  void visit (HIR::ConstantItem &constant) override
  {
    dump += indent () + "constant " + constant.get_identifier () + ":"
	    + type_string (constant.get_mappings ()) + " = ";
    constant.get_expr ()->accept_vis (*this);
    dump += ";\n";
  }

  void visit (HIR::Function &function) override
  {
    dump += indent () + "fn " + function.get_function_name () + " "
	    + type_string (function.get_mappings ()) + "\n";
    dump += indent () + "{\n";

    HIR::BlockExpr *function_body = function.get_definition ().get ();
    function_body->accept_vis (*this);

    dump += indent () + "}\n";
  }

  void visit (HIR::BlockExpr &expr) override
  {
    dump += "{\n";
    indentation_level++;

    for (auto &s : expr.get_statements ())
      {
	dump += indent ();
	s->accept_vis (*this);
	dump += ";\n";
      }

    if (expr.has_expr ())
      {
	dump += indent ();
	expr.expr->accept_vis (*this);
	dump += ";\n";
      }

    indentation_level--;
    dump += "}\n";
  }

  void visit (HIR::UnsafeBlockExpr &expr) override
  {
    dump += "unsafe ";
    expr.get_block_expr ()->accept_vis (*this);
  }

  void visit (HIR::LetStmt &stmt) override
  {
    dump += "let " + stmt.get_pattern ()->as_string () + ":"
	    + type_string (stmt.get_pattern ()->get_pattern_mappings ());
    if (stmt.has_init_expr ())
      {
	dump += " = ";
	stmt.get_init_expr ()->accept_vis (*this);
      }
  }

  void visit (HIR::ExprStmtWithBlock &stmt) override
  {
    stmt.get_expr ()->accept_vis (*this);
  }

  void visit (HIR::ExprStmtWithoutBlock &stmt) override
  {
    stmt.get_expr ()->accept_vis (*this);
  }

  void visit (HIR::AssignmentExpr &expr) override
  {
    expr.get_lhs ()->accept_vis (*this);
    dump += " = ";
    expr.get_rhs ()->accept_vis (*this);
  }

  void visit (HIR::LiteralExpr &expr) override
  {
    dump += expr.get_literal ().as_string () + ":"
	    + type_string (expr.get_mappings ());
  }

  void visit (HIR::ArrayExpr &expr) override
  {
    dump += type_string (expr.get_mappings ()) + ":[";

    HIR::ArrayElems *elements = expr.get_internal_elements ();
    elements->accept_vis (*this);

    dump += "]";
  }

  void visit (HIR::ArrayElemsValues &elems) override
  {
    for (auto &elem : elems.get_values ())
      {
	elem->accept_vis (*this);
	dump += ",";
      }
  }

  void visit (HIR::GroupedExpr &expr) override
  {
    HIR::Expr *paren_expr = expr.get_expr_in_parens ().get ();
    dump += "(";
    paren_expr->accept_vis (*this);
    dump += ")";
  }

  void visit (HIR::PathInExpression &expr) override
  {
    dump += type_string (expr.get_mappings ());
  }

  void visit (HIR::StructExprStructFields &expr) override
  {
    dump += "ctor: " + type_string (expr.get_mappings ());
  }

protected:
  std::string type_string (const Analysis::NodeMapping &mappings)
  {
    TyTy::BaseType *lookup = nullptr;
    if (!context->lookup_type (mappings.get_hirid (), &lookup))
      return "<error>";

    std::string buf = "[";
    for (auto &ref : lookup->get_combined_refs ())
      {
	buf += std::to_string (ref);
	buf += ", ";
      }
    buf += "]";

    std::string repr = lookup->as_string ();
    return "<" + repr + " HIRID: " + std::to_string (mappings.get_hirid ())
	   + " RF:" + std::to_string (lookup->get_ref ()) + " TF:"
	   + std::to_string (lookup->get_ty_ref ()) + +" - " + buf + ">";
  }

  std::string indent ()
  {
    std::string buf;
    for (size_t i = 0; i < indentation_level; ++i)
      buf += "    ";

    return buf;
  }

private:
  TypeResolverDump () : TypeCheckBase (), indentation_level (0) {}

  std::string dump;
  size_t indentation_level;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_TYCHECK_DUMP
