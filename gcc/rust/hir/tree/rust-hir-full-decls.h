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

#ifndef RUST_HIR_FULL_DECLS_H
#define RUST_HIR_FULL_DECLS_H

namespace Rust {
namespace HIR {

struct Literal;
class Stmt;
class Item;
class Expr;
class ExprWithoutBlock;
class Pattern;
class Type;
class TypeNoBounds;
class TypeParamBound;
class Lifetime;
class GenericParam;
class LifetimeParam;

class TraitItem;
class ImplItem;
class Crate;
class PathExpr;

// rust-path.h
class PathIdentSegment;
class GenericArgsBinding;
class GenericArgs;
class PathExprSegment;
class PathPattern;
class PathInExpression;
class TypePathSegment;
class TypePathSegmentGeneric;
class TypePathFunction;
class TypePathSegmentFunction;
class TypePath;
class QualifiedPathType;
class QualifiedPathInExpression;
class QualifiedPathInType;

// rust-expr.h
class ExprWithBlock;
class LiteralExpr;
class AttrInputLiteral;
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
class BlockExpr;
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
class IfExpr;
class IfExprConseqElse;
class IfLetExpr;
class IfLetExprConseqElse;
struct MatchArm;
// class MatchCase;
// class MatchCaseBlockExpr;
// class MatchCaseExpr;
struct MatchCase;
class MatchExpr;
class AwaitExpr;
class AsyncBlockExpr;
class InlineAsmReg;
class InlineAsmRegClass;
struct InlineAsmRegOrRegClass;
class InlineAsm;

// rust-stmt.h
class EmptyStmt;
class LetStmt;
class ExprStmt;

// rust-item.h
class TypeParam;
class ConstGenericParam;
class WhereClauseItem;
class LifetimeWhereClauseItem;
class TypeBoundWhereClauseItem;
struct WhereClause;
struct SelfParam;
struct FunctionQualifiers;
struct FunctionParam;
struct Visibility;
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
class StructField;
class StructStruct;
class TupleField;
class TupleStruct;
class EnumItem;
class EnumItemTuple;
class EnumItemStruct;
class EnumItemDiscriminant;
class Enum;
class Union;
class ConstantItem;
class StaticItem;
class TraitFunctionDecl;
class TraitItemFunc;
class TraitItemConst;
class TraitItemType;
class Trait;
class ImplBlock;
class ExternalItem;
class ExternalStaticItem;
struct NamedFunctionParam;
class ExternalFunctionItem;
class ExternBlock;

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
class StructPattern;
class TupleStructItems;
class TupleStructItemsNoRange;
class TupleStructItemsRange;
class TupleStructPattern;
class TuplePatternItems;
class TuplePatternItemsMultiple;
class TuplePatternItemsRanged;
class TuplePattern;
class SlicePattern;
class AltPattern;

// rust-type.h
class TraitBound;
class ImplTraitType;
class TraitObjectType;
class ParenthesisedType;
class ImplTraitTypeOneBound;
class TupleType;
class NeverType;
class RawPointerType;
class ReferenceType;
class ArrayType;
class SliceType;
class InferredType;
struct MaybeNamedParam;
class BareFunctionType;
} // namespace HIR
} // namespace Rust

#endif
