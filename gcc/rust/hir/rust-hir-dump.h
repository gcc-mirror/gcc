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

#ifndef RUST_HIR_DUMP_H
#define RUST_HIR_DUMP_H

#include "rust-hir-expr.h"
#include "rust-hir-item.h"
#include "rust-hir-visitor.h"
#include "rust-hir.h"
#include "rust-hir-full.h"
#include "rust-dump.h"

namespace Rust {
namespace HIR {

class Dump : public HIRFullVisitor
{
public:
  static void debug (FullVisitable &v);

  Dump (std::ostream &stream);
  void go (HIR::Crate &crate);

private:
  bool beg_of_line;
  Indent indentation;
  std::ostream &stream;

  void put (std::string name, bool newline = true);

  enum delim
  {
    CURLY = 0,
    SQUARE = 1,
  };

  static std::string delims[2][2];

  void begin (std::string name, enum delim = SQUARE);
  void end (std::string name, enum delim = SQUARE);
  void begin_field (std::string name);
  void end_field (std::string name);

  template <class T>
  void visit_collection (std::string name,
			 std::vector<std::unique_ptr<T>> &vec);

  template <class T>
  void visit_collection (std::string name, std::vector<T> &vec);

  void visit_field (std::string field_name, FullVisitable &v);

  template <class T>
  void visit_field (std::string field_name, std::unique_ptr<T> &);

  void put_field (std::string field_name, std::string text);
  void do_vis_item (VisItem &);
  void do_mappings (const Analysis::NodeMapping &mappings);
  void do_inner_attrs (WithInnerAttrs &);
  void do_outer_attrs (std::vector<AST::Attribute> &attrs);

  void do_stmt (Stmt &);
  void do_item (Item &);
  void do_type (Type &);
  void do_expr (Expr &);
  void do_ifexpr (IfExpr &);
  void do_pathexpr (PathExpr &);
  void do_pathpattern (PathPattern &);
  void do_genericargs (GenericArgs &);
  void do_typepathsegment (TypePathSegment &);
  void do_typepathfunction (TypePathFunction &);
  void do_externalitem (ExternalItem &);
  void do_operatorexpr (OperatorExpr &);
  void do_structexprstruct (StructExprStruct &);
  void do_functionparam (FunctionParam &);
  void do_qualifiedpathtype (QualifiedPathType &);
  void do_baseloopexpr (BaseLoopExpr &);
  void do_traititem (TraitItem &);
  void do_traitfunctiondecl (TraitFunctionDecl &);
  void do_namefunctionparam (NamedFunctionParam &);
  void do_enumitem (EnumItem &);
  void do_tuplefield (TupleField &);
  void do_structfield (StructField &);
  void do_maybenamedparam (MaybeNamedParam &);
  void do_struct (Struct &);
  void do_matcharm (MatchArm &);
  void do_matchcase (MatchCase &);

  void visit (AST::Attribute &attribute);
  virtual void visit (Lifetime &) override;
  virtual void visit (LifetimeParam &) override;
  virtual void visit (PathInExpression &) override;
  virtual void visit (TypePathSegment &) override;
  virtual void visit (TypePathSegmentGeneric &) override;
  virtual void visit (TypePathSegmentFunction &) override;
  virtual void visit (TypePath &) override;
  virtual void visit (QualifiedPathInExpression &) override;
  virtual void visit (QualifiedPathInType &) override;

  virtual void visit (LiteralExpr &) override;
  virtual void visit (BorrowExpr &) override;
  virtual void visit (DereferenceExpr &) override;
  virtual void visit (ErrorPropagationExpr &) override;
  virtual void visit (NegationExpr &) override;
  virtual void visit (ArithmeticOrLogicalExpr &) override;
  virtual void visit (ComparisonExpr &) override;
  virtual void visit (LazyBooleanExpr &) override;
  virtual void visit (TypeCastExpr &) override;
  virtual void visit (AssignmentExpr &) override;
  virtual void visit (CompoundAssignmentExpr &) override;
  virtual void visit (GroupedExpr &) override;

  virtual void visit (ArrayElemsValues &) override;
  virtual void visit (ArrayElemsCopied &) override;
  virtual void visit (ArrayExpr &) override;
  virtual void visit (ArrayIndexExpr &) override;
  virtual void visit (TupleExpr &) override;
  virtual void visit (TupleIndexExpr &) override;
  virtual void visit (StructExprStruct &) override;

  virtual void visit (StructExprFieldIdentifier &) override;
  virtual void visit (StructExprFieldIdentifierValue &) override;

  virtual void visit (StructExprFieldIndexValue &) override;
  virtual void visit (StructExprStructFields &) override;
  virtual void visit (StructExprStructBase &) override;

  virtual void visit (CallExpr &) override;
  virtual void visit (MethodCallExpr &) override;
  virtual void visit (FieldAccessExpr &) override;
  virtual void visit (ClosureExpr &) override;
  virtual void visit (BlockExpr &) override;
  virtual void visit (ContinueExpr &) override;
  virtual void visit (BreakExpr &) override;
  virtual void visit (RangeFromToExpr &) override;
  virtual void visit (RangeFromExpr &) override;
  virtual void visit (RangeToExpr &) override;
  virtual void visit (RangeFullExpr &) override;
  virtual void visit (RangeFromToInclExpr &) override;
  virtual void visit (RangeToInclExpr &) override;
  virtual void visit (ReturnExpr &) override;
  virtual void visit (UnsafeBlockExpr &) override;
  virtual void visit (LoopExpr &) override;
  virtual void visit (WhileLoopExpr &) override;
  virtual void visit (WhileLetLoopExpr &) override;
  virtual void visit (IfExpr &) override;
  virtual void visit (IfExprConseqElse &) override;

  virtual void visit (MatchExpr &) override;
  virtual void visit (AwaitExpr &) override;
  virtual void visit (AsyncBlockExpr &) override;
  virtual void visit (InlineAsm &) override;

  virtual void visit (TypeParam &) override;
  virtual void visit (ConstGenericParam &) override;

  virtual void visit (LifetimeWhereClauseItem &) override;
  virtual void visit (TypeBoundWhereClauseItem &) override;
  virtual void visit (Module &) override;
  virtual void visit (ExternCrate &) override;

  virtual void visit (UseTreeGlob &) override;
  virtual void visit (UseTreeList &) override;
  virtual void visit (UseTreeRebind &) override;
  virtual void visit (UseDeclaration &) override;
  virtual void visit (Function &) override;
  virtual void visit (TypeAlias &) override;
  virtual void visit (StructStruct &) override;
  virtual void visit (TupleStruct &) override;

  virtual void visit (EnumItem &) override;
  virtual void visit (EnumItemTuple &) override;
  virtual void visit (EnumItemStruct &) override;
  virtual void visit (EnumItemDiscriminant &) override;

  virtual void visit (Enum &) override;
  virtual void visit (Union &) override;
  virtual void visit (ConstantItem &) override;
  virtual void visit (StaticItem &) override;
  virtual void visit (TraitItemFunc &) override;
  virtual void visit (TraitItemConst &) override;
  virtual void visit (TraitItemType &) override;
  virtual void visit (Trait &) override;
  virtual void visit (ImplBlock &) override;

  virtual void visit (ExternalStaticItem &) override;
  virtual void visit (ExternalFunctionItem &) override;
  virtual void visit (ExternalTypeItem &) override;
  virtual void visit (ExternBlock &) override;

  virtual void visit (LiteralPattern &) override;
  virtual void visit (IdentifierPattern &) override;
  virtual void visit (WildcardPattern &) override;

  virtual void visit (RangePatternBoundLiteral &) override;
  virtual void visit (RangePatternBoundPath &) override;
  virtual void visit (RangePatternBoundQualPath &) override;
  virtual void visit (RangePattern &) override;
  virtual void visit (ReferencePattern &) override;

  virtual void visit (StructPatternFieldTuplePat &) override;
  virtual void visit (StructPatternFieldIdentPat &) override;
  virtual void visit (StructPatternFieldIdent &) override;
  virtual void visit (StructPattern &) override;

  virtual void visit (TupleStructItemsNoRange &) override;
  virtual void visit (TupleStructItemsRange &) override;
  virtual void visit (TupleStructPattern &) override;

  virtual void visit (TuplePatternItemsMultiple &) override;
  virtual void visit (TuplePatternItemsRanged &) override;
  virtual void visit (TuplePattern &) override;
  virtual void visit (SlicePattern &) override;
  virtual void visit (AltPattern &) override;

  virtual void visit (EmptyStmt &) override;
  virtual void visit (LetStmt &) override;
  virtual void visit (ExprStmt &) override;

  virtual void visit (TraitBound &) override;
  virtual void visit (ImplTraitType &) override;
  virtual void visit (TraitObjectType &) override;
  virtual void visit (ParenthesisedType &) override;
  virtual void visit (TupleType &) override;
  virtual void visit (NeverType &) override;
  virtual void visit (RawPointerType &) override;
  virtual void visit (ReferenceType &) override;
  virtual void visit (ArrayType &) override;
  virtual void visit (SliceType &) override;
  virtual void visit (InferredType &) override;
  virtual void visit (BareFunctionType &) override;
};

} // namespace HIR
} // namespace Rust

// In the global namespace to make it easier to call from debugger
void
debug (Rust::HIR::FullVisitable &v);

#endif // !RUST_HIR_DUMP_H
