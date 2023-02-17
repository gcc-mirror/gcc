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

#ifndef RUST_AST_FULL_DECLS_H
#define RUST_AST_FULL_DECLS_H

// Forward declarations for all AST classes. Useful for not having to include
// all definitions.

namespace Rust {
namespace AST {
// rust-ast.h
class AttrInput;
class TokenTree;
class MacroMatch;
class Token;
struct Literal;
class DelimTokenTree;
class PathSegment;
class SimplePathSegment;
class SimplePath;
struct Attribute;
class MetaItemInner;
class AttrInputMetaItemContainer;
class MetaItem;
class Stmt;
class Item;
class Expr;
class ExprWithoutBlock;
class IdentifierExpr;
class Pattern;
class Type;
class TypeNoBounds;
class TypeParamBound;
class Lifetime;
class GenericParam;
class LifetimeParam;
class ConstGenericParam;
class TraitItem;
class InherentImplItem;
class TraitImplItem;
struct Crate;
class PathExpr;

// rust-path.h
class PathIdentSegment;
struct GenericArgsBinding;
struct GenericArgs;
class PathExprSegment;
class PathPattern;
class PathInExpression;
class TypePathSegment;
class TypePathSegmentGeneric;
struct TypePathFunction;
class TypePathSegmentFunction;
class TypePath;
struct QualifiedPathType;
class QualifiedPathInExpression;
class QualifiedPathInType;

// rust-expr.h
class ExprWithBlock;
class LiteralExpr;
class AttrInputLiteral;
class MetaItemLitExpr;
class MetaItemPathLit;
class OperatorExpr;
class BorrowExpr;
class DereferenceExpr;
class ErrorPropagationExpr;
class NegationExpr;
class ArithmeticOrLogicalExpr;
class ComparisonExpr;
class LazyBooleanExpr;
class TypeCastExpr;
class AssignmentExpr;
class CompoundAssignmentExpr;
class GroupedExpr;
class ArrayElems;
class ArrayElemsValues;
class ArrayElemsCopied;
class ArrayExpr;
class ArrayIndexExpr;
class TupleExpr;
class TupleIndexExpr;
class StructExpr;
class StructExprStruct;
struct StructBase;
class StructExprField;
class StructExprFieldIdentifier;
class StructExprFieldWithVal;
class StructExprFieldIdentifierValue;
class StructExprFieldIndexValue;
class StructExprStructFields;
class StructExprStructBase;
class CallExpr;
class MethodCallExpr;
class FieldAccessExpr;
struct ClosureParam;
class ClosureExpr;
class ClosureExprInner;
class BlockExpr;
class ClosureExprInnerTyped;
class ContinueExpr;
class BreakExpr;
class RangeExpr;
class RangeFromToExpr;
class RangeFromExpr;
class RangeToExpr;
class RangeFullExpr;
class RangeFromToInclExpr;
class RangeToInclExpr;
class ReturnExpr;
class UnsafeBlockExpr;
class LoopLabel;
class BaseLoopExpr;
class LoopExpr;
class WhileLoopExpr;
class WhileLetLoopExpr;
class ForLoopExpr;
class IfExpr;
class IfExprConseqElse;
class IfExprConseqIf;
class IfLetExpr;
class IfExprConseqIfLet;
class IfLetExprConseqElse;
class IfLetExprConseqIf;
class IfLetExprConseqIfLet;
struct MatchArm;
// class MatchCase;
// class MatchCaseBlockExpr;
// class MatchCaseExpr;
struct MatchCase;
class MatchExpr;
class AwaitExpr;
class AsyncBlockExpr;
class InlineAsm;

// rust-stmt.h
class EmptyStmt;
class LetStmt;
class ExprStmt;
class ExprStmtWithoutBlock;
class ExprStmtWithBlock;

// rust-item.h
class TypeParam;
class WhereClauseItem;
class LifetimeWhereClauseItem;
class TypeBoundWhereClauseItem;
struct WhereClause;
struct SelfParam;
struct FunctionQualifiers;
struct FunctionParam;
struct Visibility;
class Method;
class VisItem;
class Module;
class ExternCrate;
class UseTree;
class UseTreeGlob;
class UseTreeList;
class UseTreeRebind;
class UseDeclaration;
class Function;
class TypeAlias;
class Struct;
struct StructField;
class StructStruct;
struct TupleField;
class TupleStruct;
class EnumItem;
class EnumItemTuple;
class EnumItemStruct;
class EnumItemDiscriminant;
class Enum;
class Union;
class ConstantItem;
class StaticItem;
struct TraitFunctionDecl;
class TraitItemFunc;
struct TraitMethodDecl;
class TraitItemMethod;
class TraitItemConst;
class TraitItemType;
class Trait;
class Impl;
class InherentImpl;
class TraitImpl;
class ExternalItem;
class ExternalStaticItem;
struct NamedFunctionParam;
class ExternalFunctionItem;
class ExternBlock;

// rust-macro.h
class MacroMatchFragment;
class MacroMatchRepetition;
class MacroMatcher;
struct MacroTranscriber;
struct MacroRule;
class MacroRulesDefinition;
class MacroInvocation;
class MetaItemPath;
class MetaItemSeq;
class MetaWord;
class MetaNameValueStr;
class MetaListPaths;
class MetaListNameValueStr;

// rust-pattern.h
class LiteralPattern;
class IdentifierPattern;
class WildcardPattern;
class RangePatternBound;
class RangePatternBoundLiteral;
class RangePatternBoundPath;
class RangePatternBoundQualPath;
class RangePattern;
class ReferencePattern;
struct StructPatternEtc;
class StructPatternField;
class StructPatternFieldTuplePat;
class StructPatternFieldIdentPat;
class StructPatternFieldIdent;
struct StructPatternElements;
class StructPattern;
class TupleStructItems;
class TupleStructItemsNoRange;
class TupleStructItemsRange;
class TupleStructPattern;
class TuplePatternItems;
class TuplePatternItemsMultiple;
class TuplePatternItemsRanged;
class TuplePattern;
class GroupedPattern;
class SlicePattern;
class AltPattern;

// rust-type.h
class TraitBound;
class ImplTraitType;
class TraitObjectType;
class ParenthesisedType;
class ImplTraitTypeOneBound;
class TraitObjectTypeOneBound;
class TupleType;
class NeverType;
class RawPointerType;
class ReferenceType;
class ArrayType;
class SliceType;
class InferredType;
struct MaybeNamedParam;
class BareFunctionType;
} // namespace AST
} // namespace Rust

#endif
