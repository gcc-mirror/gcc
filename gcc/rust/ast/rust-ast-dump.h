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

#include "rust-ast-visitor.h"
#include "rust-ast.h"
#include "rust-ast-full.h"

#ifndef RUST_AST_DUMP_H
#define RUST_AST_DUMP_H

namespace Rust {
namespace AST {

// TODO: We might want to reuse this class somewhere else
class Indent
{
public:
  Indent ();

  friend std::ostream &operator<< (std::ostream &stream, const Indent &indent);

  void increment ();
  void decrement ();

private:
  size_t tabs;
};

class Dump : public ASTVisitor
{
public:
  Dump (std::ostream &stream);

  /**
   * Run the visitor on an entire crate and its items
   */
  void go (AST::Crate &crate);
  void go (AST::Item &item);

  /**
   * Use the AST Dump as a debugging tool
   */
  template <typename T> static void debug (T &instance)
  {
    auto dump = Dump (std::cerr);

    std::cerr << '\n';
    instance.accept_vis (dump);
    std::cerr << '\n';
  }
  template <typename T> static void debug (std::unique_ptr<T> &instance)
  {
    debug (*instance);
  }

private:
  std::ostream &stream;
  Indent indentation;

  /**
   * Compatibility layer for using the visitor pattern on polymorphic classes
   * with a unified overload syntax. This allows us to call `visit` both on
   * types implementing `accept_vis` method and for classes for which the
   * `visit` method is directly implemented.
   */
  template <typename T> void visit (std::unique_ptr<T> &node);

  /**
   * @see visit<std::unique_ptr<T>>
   */
  template <typename T> void visit (T &node);

  /**
   * Visit all items in given @collection, placing the separator in between but
   * not at the end.
   * Start and end offset allow to visit only a "slice" from the collection.
   */
  template <typename T>
  void visit_items_joined_by_separator (T &collection,
					const std::string &separator = "",
					size_t start_offset = 0,
					size_t end_offset = 0);

  /**
   * Visit item placing indentation before and trailing string + end of line
   * after.
   */
  template <typename T>
  void visit_as_line (T &item, const std::string &trailing = "");

  /**
   * Visit each item in @collection "as line".
   *
   * @see visit_as_line
   */
  template <typename T>
  void visit_items_as_lines (T &collection, const std::string &trailing = "");

  /**
   * Visit each item in @collection as lines inside a block delimited by braces
   * with increased indentation. Also includes special handling for empty
   * collection to print only the delimiters with no new line inside.
   */
  template <typename T>
  void visit_items_as_block (T &collection, const std::string &line_trailing,
			     char left_brace = '{', char right_brace = '}');

  /**
   * Visit common items of functions: Parameters, return type, block
   */
  void visit_function_common (std::unique_ptr<Type> &return_type,
			      std::unique_ptr<BlockExpr> &block);

  void visit (FunctionParam &param);
  void visit (Attribute &attrib);
  void visit (Visibility &vis);
  void visit (std::vector<std::unique_ptr<GenericParam>> &params);
  void visit (TupleField &field);
  void visit (StructField &field);
  void visit (SimplePathSegment &segment);
  void visit (NamedFunctionParam &param);
  void visit (MacroRule &rule);
  void visit (WhereClause &rule);
  void visit (std::vector<LifetimeParam> &for_lifetimes);
  void visit (FunctionQualifiers &qualifiers);
  void visit (MaybeNamedParam &param);
  void visit (TypePathFunction &type_path_fn);
  void visit (GenericArgsBinding &binding);
  void visit (GenericArg &arg);

  // rust-ast.h
  void visit (Token &tok);
  void visit (DelimTokenTree &delim_tok_tree);
  void visit (AttrInputMetaItemContainer &input);
  void visit (IdentifierExpr &ident_expr);
  void visit (Lifetime &lifetime);
  void visit (LifetimeParam &lifetime_param);
  void visit (ConstGenericParam &const_param);

  // rust-path.h
  void visit (PathInExpression &path);
  void visit (TypePathSegment &segment);
  void visit (TypePathSegmentGeneric &segment);
  void visit (TypePathSegmentFunction &segment);
  void visit (TypePath &path);
  void visit (QualifiedPathInExpression &path);
  void visit (QualifiedPathInType &path);

  // rust-expr.h
  void visit (LiteralExpr &expr);
  void visit (AttrInputLiteral &attr_input);
  void visit (MetaItemLitExpr &meta_item);
  void visit (MetaItemPathLit &meta_item);
  void visit (BorrowExpr &expr);
  void visit (DereferenceExpr &expr);
  void visit (ErrorPropagationExpr &expr);
  void visit (NegationExpr &expr);
  void visit (ArithmeticOrLogicalExpr &expr);
  void visit (ComparisonExpr &expr);
  void visit (LazyBooleanExpr &expr);
  void visit (TypeCastExpr &expr);
  void visit (AssignmentExpr &expr);
  void visit (CompoundAssignmentExpr &expr);
  void visit (GroupedExpr &expr);
  void visit (ArrayElemsValues &elems);
  void visit (ArrayElemsCopied &elems);
  void visit (ArrayExpr &expr);
  void visit (ArrayIndexExpr &expr);
  void visit (TupleExpr &expr);
  void visit (TupleIndexExpr &expr);
  void visit (StructExprStruct &expr);
  void visit (StructExprFieldIdentifier &field);
  void visit (StructExprFieldIdentifierValue &field);
  void visit (StructExprFieldIndexValue &field);
  void visit (StructExprStructFields &expr);
  void visit (StructExprStructBase &expr);
  void visit (CallExpr &expr);
  void visit (MethodCallExpr &expr);
  void visit (FieldAccessExpr &expr);
  void visit (ClosureExprInner &expr);
  void visit (BlockExpr &expr);
  void visit (ClosureExprInnerTyped &expr);
  void visit (ContinueExpr &expr);
  void visit (BreakExpr &expr);
  void visit (RangeFromToExpr &expr);
  void visit (RangeFromExpr &expr);
  void visit (RangeToExpr &expr);
  void visit (RangeFullExpr &expr);
  void visit (RangeFromToInclExpr &expr);
  void visit (RangeToInclExpr &expr);
  void visit (ReturnExpr &expr);
  void visit (UnsafeBlockExpr &expr);
  void visit (LoopExpr &expr);
  void visit (WhileLoopExpr &expr);
  void visit (WhileLetLoopExpr &expr);
  void visit (ForLoopExpr &expr);
  void visit (IfExpr &expr);
  void visit (IfExprConseqElse &expr);
  void visit (IfExprConseqIf &expr);
  void visit (IfExprConseqIfLet &expr);
  void visit (IfLetExpr &expr);
  void visit (IfLetExprConseqElse &expr);
  void visit (IfLetExprConseqIf &expr);
  void visit (IfLetExprConseqIfLet &expr);
  void visit (MatchExpr &expr);
  void visit (AwaitExpr &expr);
  void visit (AsyncBlockExpr &expr);

  // rust-item.h
  void visit (TypeParam &param);
  void visit (LifetimeWhereClauseItem &item);
  void visit (TypeBoundWhereClauseItem &item);
  void visit (Method &method);
  void visit (Module &module);
  void visit (ExternCrate &crate);
  void visit (UseTreeGlob &use_tree);
  void visit (UseTreeList &use_tree);
  void visit (UseTreeRebind &use_tree);
  void visit (UseDeclaration &use_decl);
  void visit (Function &function);
  void visit (TypeAlias &type_alias);
  void visit (StructStruct &struct_item);
  void visit (TupleStruct &tuple_struct);
  void visit (EnumItem &item);
  void visit (EnumItemTuple &item);
  void visit (EnumItemStruct &item);
  void visit (EnumItemDiscriminant &item);
  void visit (Enum &enum_item);
  void visit (Union &union_item);
  void visit (ConstantItem &const_item);
  void visit (StaticItem &static_item);
  void visit (TraitItemFunc &item);
  void visit (TraitItemMethod &item);
  void visit (TraitItemConst &item);
  void visit (TraitItemType &item);
  void visit (Trait &trait);
  void visit (InherentImpl &impl);
  void visit (TraitImpl &impl);
  void visit (ExternalStaticItem &item);
  void visit (ExternalFunctionItem &item);
  void visit (ExternBlock &block);

  // rust-macro.h
  void visit (MacroMatchFragment &match);
  void visit (MacroMatchRepetition &match);
  void visit (MacroMatcher &matcher);
  void visit (MacroRulesDefinition &rules_def);
  void visit (MacroInvocation &macro_invoc);
  void visit (MetaItemPath &meta_item);
  void visit (MetaItemSeq &meta_item);
  void visit (MetaWord &meta_item);
  void visit (MetaNameValueStr &meta_item);
  void visit (MetaListPaths &meta_item);
  void visit (MetaListNameValueStr &meta_item);

  // rust-pattern.h
  void visit (LiteralPattern &pattern);
  void visit (IdentifierPattern &pattern);
  void visit (WildcardPattern &pattern);
  // void visit(RangePatternBound& bound);
  void visit (RangePatternBoundLiteral &bound);
  void visit (RangePatternBoundPath &bound);
  void visit (RangePatternBoundQualPath &bound);
  void visit (RangePattern &pattern);
  void visit (ReferencePattern &pattern);
  // void visit(StructPatternField& field);
  void visit (StructPatternFieldTuplePat &field);
  void visit (StructPatternFieldIdentPat &field);
  void visit (StructPatternFieldIdent &field);
  void visit (StructPattern &pattern);
  // void visit(TupleStructItems& tuple_items);
  void visit (TupleStructItemsNoRange &tuple_items);
  void visit (TupleStructItemsRange &tuple_items);
  void visit (TupleStructPattern &pattern);
  // void visit(TuplePatternItems& tuple_items);
  void visit (TuplePatternItemsMultiple &tuple_items);
  void visit (TuplePatternItemsRanged &tuple_items);
  void visit (TuplePattern &pattern);
  void visit (GroupedPattern &pattern);
  void visit (SlicePattern &pattern);
  void visit (AltPattern &pattern);

  // rust-stmt.h
  void visit (EmptyStmt &stmt);
  void visit (LetStmt &stmt);
  void visit (ExprStmtWithoutBlock &stmt);
  void visit (ExprStmtWithBlock &stmt);

  // rust-type.h
  void visit (TraitBound &bound);
  void visit (ImplTraitType &type);
  void visit (TraitObjectType &type);
  void visit (ParenthesisedType &type);
  void visit (ImplTraitTypeOneBound &type);
  void visit (TraitObjectTypeOneBound &type);
  void visit (TupleType &type);
  void visit (NeverType &type);
  void visit (RawPointerType &type);
  void visit (ReferenceType &type);
  void visit (ArrayType &type);
  void visit (SliceType &type);
  void visit (InferredType &type);
  void visit (BareFunctionType &type);
};

} // namespace AST
} // namespace Rust

#endif // !RUST_AST_DUMP_H
