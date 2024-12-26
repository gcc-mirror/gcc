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

#ifndef RUST_AST_COLLECTOR_H
#define RUST_AST_COLLECTOR_H

#include "rust-token.h"
#include "rust-ast-visitor.h"
#include "rust-ast.h"
#include "rust-ast-full.h"

namespace Rust {
namespace AST {

class CollectItem
{
public:
  enum class Kind
  {
    Comment,
    Newline,
    Indentation,
    Token,
  };

  CollectItem (TokenPtr token) : token (token), kind (Kind::Token) {}
  CollectItem (std::string comment) : comment (comment), kind (Kind::Comment) {}
  CollectItem (Kind kind) : kind (kind) { rust_assert (kind != Kind::Token); }
  CollectItem (size_t level) : indent_level (level), kind (Kind::Indentation) {}

  Kind get_kind () { return kind; }

  TokenPtr get_token ()
  {
    rust_assert (kind == Kind::Token);
    return token;
  }

  std::string get_comment ()
  {
    rust_assert (kind == Kind::Comment);
    return comment;
  }

  size_t get_indent_level ()
  {
    rust_assert (kind == Kind::Indentation);
    return indent_level;
  }

private:
  TokenPtr token;
  std::string comment;
  size_t indent_level;
  Kind kind;
};

class TokenCollector : public ASTVisitor
{
public:
  TokenCollector () : indent_level (0) {}

  bool output_trailing_commas = false;

  void visit (AST::Crate &crate);
  void visit (AST::Item &item);

  std::vector<TokenPtr> collect_tokens () const;
  std::vector<CollectItem> collect () const;

private:
  std::vector<CollectItem> tokens;
  size_t indent_level;

  void push (TokenPtr token) { tokens.push_back ({token}); }

  /**
   * Visit all items in given @collection, placing the separator in between but
   * not at the end.
   */
  template <typename T>
  void visit_items_joined_by_separator (T &collection,
					TokenId separator = COMMA,
					size_t start_offset = 0,
					size_t end_offset = 0)
  {
    if (collection.size () > start_offset)
      {
	visit (collection.at (start_offset));
	auto size = collection.size () - end_offset;
	for (size_t i = start_offset + 1; i < size; i++)
	  {
	    push (Rust::Token::make (separator, UNDEF_LOCATION));
	    visit (collection.at (i));
	  }
      }
  }

  /**
   * Visit item placing end of line after.
   */
  template <typename T>
  void visit_as_line (T &item, std::vector<TokenPtr> trailing = {})
  {
    indentation ();
    visit (item);
    for (auto &token : trailing)
      push (token);
    newline ();
  }

  /**
   * Visit each item in @collection "as line".
   *
   * @see visit_as_line
   */
  template <typename T>
  void visit_items_as_lines (T &collection, std::vector<TokenPtr> trailing = {})
  {
    for (auto &item : collection)
      visit_as_line (item, trailing);
  }

  /**
   * Visit each item in @collection as lines inside a block delimited by braces
   * with increased indentation. Also includes special handling for empty
   * collection to print only the delimiters with no new line inside.
   */
  template <typename T>
  void visit_items_as_block (T &collection, std::vector<TokenPtr> trailing = {},
			     TokenId left_brace = LEFT_CURLY,
			     TokenId right_brace = RIGHT_CURLY)
  {
    push (Rust::Token::make (left_brace, UNDEF_LOCATION));
    if (collection.empty ())
      {
	push (Rust::Token::make (right_brace, UNDEF_LOCATION));
	newline ();
      }
    else
      {
	newline ();
	increment_indentation ();
	visit_items_as_lines (collection, trailing);
	decrement_indentation ();
	indentation ();
	push (Rust::Token::make (right_brace, UNDEF_LOCATION));
	newline ();
      }
  }

  void trailing_comma ();
  void newline ();
  void indentation ();
  void increment_indentation ();
  void decrement_indentation ();
  void comment (std::string comment);
  /**
   * Visit common items of functions: Parameters, return type, block
   */
  void visit_function_common (std::unique_ptr<Type> &return_type,
			      std::unique_ptr<BlockExpr> &block);

  void visit_closure_common (ClosureExpr &expr);

  void visit_loop_common (BaseLoopExpr &expr);

public:
  /**
   * Compatibility layer for using the visitor pattern on polymorphic classes
   * with a unified overload syntax. This allows us to call `visit` both on
   * types implementing `accept_vis` method and for classes for which the
   * `visit` method is directly implemented.
   */
  template <typename T> void visit (std::unique_ptr<T> &node)
  {
    node->accept_vis (*this);
  }

  /**
   * @see visit<std::unique_ptr<T>>
   */
  template <typename T> void visit (T &node) { node.accept_vis (*this); }

  void visit (Visitable &v);
  void visit (LoopLabel &label);

  void visit (Literal &lit, location_t locus = UNDEF_LOCATION);

  void visit (FunctionParam &param);
  void visit (VariadicParam &param);
  void visit (Attribute &attrib);
  void visit (Visibility &vis);
  void visit (std::vector<std::unique_ptr<GenericParam>> &params);
  void visit (TupleField &field);
  void visit (StructField &field);
  void visit (SimplePathSegment &segment);
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
  void visit (SimplePath &path);
  void visit (PathExprSegment &segment);
  void visit (PathIdentSegment &segment);
  void visit (PathInExpression &path);
  void visit (TypePathSegment &segment);
  void visit (TypePathSegmentGeneric &segment);
  void visit (TypePathSegmentFunction &segment);
  void visit (TypePath &path);
  void visit (QualifiedPathType &path);
  void visit (QualifiedPathInExpression &path);
  void visit (QualifiedPathInType &path);

  // rust-expr.h
  void visit (LiteralExpr &expr);
  void visit (AttrInputLiteral &attr_input);
  void visit (AttrInputMacro &attr_input);
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
  void visit (StructBase &base);
  void visit (StructExprStructFields &expr);
  void visit (StructExprStructBase &expr);
  void visit (CallExpr &expr);
  void visit (MethodCallExpr &expr);
  void visit (FieldAccessExpr &expr);
  void visit (ClosureParam &param);
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
  void visit (BoxExpr &expr);
  void visit (UnsafeBlockExpr &expr);
  void visit (LoopExpr &expr);
  void visit (WhileLoopExpr &expr);
  void visit (WhileLetLoopExpr &expr);
  void visit (ForLoopExpr &expr);
  void visit (IfExpr &expr);
  void visit (IfExprConseqElse &expr);
  void visit (IfLetExpr &expr);
  void visit (IfLetExprConseqElse &expr);
  void visit (MatchArm &arm);
  void visit (MatchCase &arm);
  void visit (MatchExpr &expr);
  void visit (AwaitExpr &expr);
  void visit (AsyncBlockExpr &expr);
  void visit (InlineAsm &expr);
  // rust-item.h
  void visit (TypeParam &param);
  void visit (LifetimeWhereClauseItem &item);
  void visit (TypeBoundWhereClauseItem &item);
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
  void visit (Enum &enumeration);
  void visit (Union &union_item);
  void visit (ConstantItem &const_item);
  void visit (StaticItem &static_item);
  void visit (SelfParam &param);
  void visit (TraitItemConst &item);
  void visit (TraitItemType &item);
  void visit (Trait &trait);
  void visit (InherentImpl &impl);
  void visit (TraitImpl &impl);
  void visit (ExternalTypeItem &item);
  void visit (ExternalStaticItem &item);
  void visit (ExternBlock &block);

  // rust-macro.h
  void visit (MacroMatchFragment &match);
  void visit (MacroMatchRepetition &match);
  void visit (MacroMatcher &matcher);
  void visit (MacroRulesDefinition &rules_def);
  void visit (MacroInvocData &invoc_data);
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
  void visit (RestPattern &pattern);
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
  void visit (ExprStmt &stmt);

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

  void visit (FormatArgs &fmt);
};
} // namespace AST

} // namespace Rust

#endif // !RUST_AST_COLLECTOR_H
