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

#ifndef RUST_HIR_PATTERN_ANALYSIS_H
#define RUST_HIR_PATTERN_ANALYSIS_H

#include "rust-system.h"
#include "rust-hir-expr.h"
#include "rust-hir-type-check.h"
#include "rust-system.h"
#include "rust-tyty.h"
#include "optional.h"
#include "rust-hir-visitor.h"
#include "rust-name-resolver.h"

namespace Rust {
namespace Analysis {

using namespace HIR;

void
check_match_usefulness (Resolver::TypeCheckContext *ctx,
			TyTy::BaseType *scrutinee_ty, HIR::MatchExpr &expr);

class PatternChecker : public HIR::HIRFullVisitor
{
public:
  PatternChecker ();

  void go (HIR::Crate &crate);

private:
  Resolver::TypeCheckContext &tyctx;
  Resolver::Resolver &resolver;
  Analysis::Mappings &mappings;

  virtual void visit (Lifetime &lifetime) override;
  virtual void visit (LifetimeParam &lifetime_param) override;
  virtual void visit (PathInExpression &path) override;
  virtual void visit (TypePathSegment &segment) override;
  virtual void visit (TypePathSegmentGeneric &segment) override;
  virtual void visit (TypePathSegmentFunction &segment) override;
  virtual void visit (TypePath &path) override;
  virtual void visit (QualifiedPathInExpression &path) override;
  virtual void visit (QualifiedPathInType &path) override;
  virtual void visit (LiteralExpr &expr) override;
  virtual void visit (BorrowExpr &expr) override;
  virtual void visit (DereferenceExpr &expr) override;
  virtual void visit (ErrorPropagationExpr &expr) override;
  virtual void visit (NegationExpr &expr) override;
  virtual void visit (ArithmeticOrLogicalExpr &expr) override;
  virtual void visit (ComparisonExpr &expr) override;
  virtual void visit (LazyBooleanExpr &expr) override;
  virtual void visit (TypeCastExpr &expr) override;
  virtual void visit (AssignmentExpr &expr) override;
  virtual void visit (CompoundAssignmentExpr &expr) override;
  virtual void visit (GroupedExpr &expr) override;
  virtual void visit (ArrayElemsValues &elems) override;
  virtual void visit (ArrayElemsCopied &elems) override;
  virtual void visit (ArrayExpr &expr) override;
  virtual void visit (ArrayIndexExpr &expr) override;
  virtual void visit (TupleExpr &expr) override;
  virtual void visit (TupleIndexExpr &expr) override;
  virtual void visit (StructExprStruct &expr) override;
  virtual void visit (StructExprFieldIdentifier &field) override;
  virtual void visit (StructExprFieldIdentifierValue &field) override;
  virtual void visit (StructExprFieldIndexValue &field) override;
  virtual void visit (StructExprStructFields &expr) override;
  virtual void visit (StructExprStructBase &expr) override;
  virtual void visit (CallExpr &expr) override;
  virtual void visit (MethodCallExpr &expr) override;
  virtual void visit (FieldAccessExpr &expr) override;
  virtual void visit (BlockExpr &expr) override;
  virtual void visit (ClosureExpr &expr) override;
  virtual void visit (ContinueExpr &expr) override;
  virtual void visit (BreakExpr &expr) override;
  virtual void visit (RangeFromToExpr &expr) override;
  virtual void visit (RangeFromExpr &expr) override;
  virtual void visit (RangeToExpr &expr) override;
  virtual void visit (RangeFullExpr &expr) override;
  virtual void visit (RangeFromToInclExpr &expr) override;
  virtual void visit (RangeToInclExpr &expr) override;
  virtual void visit (ReturnExpr &expr) override;
  virtual void visit (UnsafeBlockExpr &expr) override;
  virtual void visit (LoopExpr &expr) override;
  virtual void visit (WhileLoopExpr &expr) override;
  virtual void visit (WhileLetLoopExpr &expr) override;
  virtual void visit (IfExpr &expr) override;
  virtual void visit (IfExprConseqElse &expr) override;
  virtual void visit (HIR::MatchExpr &expr) override;
  virtual void visit (AwaitExpr &expr) override;
  virtual void visit (AsyncBlockExpr &expr) override;
  virtual void visit (InlineAsm &expr) override;
  virtual void visit (TypeParam &param) override;
  virtual void visit (ConstGenericParam &param) override;
  virtual void visit (LifetimeWhereClauseItem &item) override;
  virtual void visit (TypeBoundWhereClauseItem &item) override;
  virtual void visit (Module &module) override;
  virtual void visit (ExternCrate &crate) override;
  virtual void visit (UseTreeGlob &use_tree) override;
  virtual void visit (UseTreeList &use_tree) override;
  virtual void visit (UseTreeRebind &use_tree) override;
  virtual void visit (UseDeclaration &use_decl) override;
  virtual void visit (Function &function) override;
  virtual void visit (TypeAlias &type_alias) override;
  virtual void visit (StructStruct &struct_item) override;
  virtual void visit (TupleStruct &tuple_struct) override;
  virtual void visit (EnumItem &item) override;
  virtual void visit (EnumItemTuple &item) override;
  virtual void visit (EnumItemStruct &item) override;
  virtual void visit (EnumItemDiscriminant &item) override;
  virtual void visit (Enum &enum_item) override;
  virtual void visit (Union &union_item) override;
  virtual void visit (ConstantItem &const_item) override;
  virtual void visit (StaticItem &static_item) override;
  virtual void visit (TraitItemFunc &item) override;
  virtual void visit (TraitItemConst &item) override;
  virtual void visit (TraitItemType &item) override;
  virtual void visit (Trait &trait) override;
  virtual void visit (ImplBlock &impl) override;
  virtual void visit (ExternalStaticItem &item) override;
  virtual void visit (ExternalFunctionItem &item) override;
  virtual void visit (ExternalTypeItem &item) override;
  virtual void visit (ExternBlock &block) override;
  virtual void visit (LiteralPattern &pattern) override;
  virtual void visit (IdentifierPattern &pattern) override;
  virtual void visit (WildcardPattern &pattern) override;
  virtual void visit (RangePatternBoundLiteral &bound) override;
  virtual void visit (RangePatternBoundPath &bound) override;
  virtual void visit (RangePatternBoundQualPath &bound) override;
  virtual void visit (RangePattern &pattern) override;
  virtual void visit (ReferencePattern &pattern) override;
  virtual void visit (StructPatternFieldTuplePat &field) override;
  virtual void visit (StructPatternFieldIdentPat &field) override;
  virtual void visit (StructPatternFieldIdent &field) override;
  virtual void visit (StructPattern &pattern) override;
  virtual void visit (TupleStructItemsNoRange &tuple_items) override;
  virtual void visit (TupleStructItemsRange &tuple_items) override;
  virtual void visit (TupleStructPattern &pattern) override;
  virtual void visit (TuplePatternItemsMultiple &tuple_items) override;
  virtual void visit (TuplePatternItemsRanged &tuple_items) override;
  virtual void visit (TuplePattern &pattern) override;
  virtual void visit (SlicePattern &pattern) override;
  virtual void visit (AltPattern &pattern) override;
  virtual void visit (EmptyStmt &stmt) override;
  virtual void visit (LetStmt &stmt) override;
  virtual void visit (ExprStmt &stmt) override;
  virtual void visit (TraitBound &bound) override;
  virtual void visit (ImplTraitType &type) override;
  virtual void visit (TraitObjectType &type) override;
  virtual void visit (ParenthesisedType &type) override;
  virtual void visit (TupleType &type) override;
  virtual void visit (NeverType &type) override;
  virtual void visit (RawPointerType &type) override;
  virtual void visit (ReferenceType &type) override;
  virtual void visit (ArrayType &type) override;
  virtual void visit (SliceType &type) override;
  virtual void visit (InferredType &type) override;
  virtual void visit (BareFunctionType &type) override;
};

struct IntRange
{
  int64_t lo;
  int64_t hi;
};

class Constructor
{
public:
  enum class ConstructorKind
  {
    // tuple or struct
    STRUCT,
    // enum variant
    VARIANT,
    // integers
    INT_RANGE,
    // user-provided wildcard
    WILDCARD,
    // references
    REFERENCE,
  };

  static Constructor make_wildcard ()
  {
    return Constructor (ConstructorKind::WILDCARD);
  }

  static Constructor make_reference ()
  {
    return Constructor (ConstructorKind::REFERENCE);
  }

  static Constructor make_struct ()
  {
    Constructor c (ConstructorKind::STRUCT);
    c.variant_idx = 0;
    return c;
  }

  static Constructor make_variant (int variant_idx)
  {
    Constructor c (ConstructorKind::VARIANT);
    c.variant_idx = variant_idx;
    return c;
  }

  ConstructorKind get_kind () const { return kind; }

  int get_variant_index () const
  {
    rust_assert (kind == ConstructorKind::VARIANT
		 || kind == ConstructorKind::STRUCT);
    return variant_idx;
  }

  bool is_covered_by (const Constructor &o) const;

  bool is_wildcard () const { return kind == ConstructorKind::WILDCARD; }

  // Requrired by std::set<T>
  bool operator< (const Constructor &o) const;

  std::string to_string () const;

private:
  Constructor (ConstructorKind kind) : kind (kind), variant_idx (0) {}
  ConstructorKind kind;

  union
  {
    // for enum variants, the variant index (always 0 for structs)
    int variant_idx;

    // for integer ranges, the range
    IntRange int_range;
  };
};

class DeconstructedPat
{
public:
  DeconstructedPat (Constructor ctor, int arity,
		    std::vector<DeconstructedPat> fields, location_t locus)
    : ctor (ctor), arity (arity), fields (fields)
  {}

  static DeconstructedPat make_wildcard (location_t locus)
  {
    return DeconstructedPat (Constructor::make_wildcard (), locus);
  }

  static DeconstructedPat make_reference (location_t locus)
  {
    return DeconstructedPat (Constructor::make_reference (), locus);
  }

  const Constructor &get_ctor () const { return ctor; }

  int get_arity () const { return arity; }

  std::vector<DeconstructedPat> specialize (const Constructor &other_ctor,
					    int other_ctor_arity) const;

  std::string to_string () const;

private:
  DeconstructedPat (Constructor ctor, location_t locus)
    : ctor (ctor), arity (0), locus (locus)
  {}

  Constructor ctor;
  int arity;
  std::vector<DeconstructedPat> fields;
  location_t locus;
};

class PatOrWild
{
public:
  static PatOrWild make_pattern (DeconstructedPat pat)
  {
    return PatOrWild (pat);
  }

  static PatOrWild make_wildcard () { return PatOrWild ({}); }

  bool is_wildcard () const
  {
    return !(pat.has_value () && !pat.value ().get_ctor ().is_wildcard ());
  }

  bool is_covered_by (const Constructor &c) const;

  // Returns the pattern if it is not a wildcard.
  const tl::optional<DeconstructedPat> &get_pat () const
  {
    rust_assert (pat.has_value ());
    return pat;
  }

  Constructor ctor () const
  {
    if (pat.has_value ())
      return pat.value ().get_ctor ();
    else
      return Constructor::make_wildcard ();
  }

  std::vector<PatOrWild> specialize (const Constructor &other_ctor,
				     int other_ctor_arity) const;

  std::string to_string () const;

private:
  PatOrWild (tl::optional<DeconstructedPat> pat) : pat (pat) {}

  tl::optional<DeconstructedPat> pat;
};

class PatStack
{
public:
  PatStack () : relevant (false) {}

  void push (PatOrWild pat) { pats.push_back (pat); }

  bool empty () const { return pats.empty (); }

  PatOrWild &head ()
  {
    rust_assert (!pats.empty ());
    return pats.front ();
  }

  const PatOrWild &head () const
  {
    rust_assert (!pats.empty ());
    return pats.front ();
  }

  // Only called if the head is a constructor which is convered by o.
  void pop_head_constructor (const Constructor &other_ctor,
			     int other_ctor_arity);

  const std::deque<PatOrWild> &get_subpatterns () const { return pats; }

private:
  void pop_head () { pats.pop_front (); }

  std::deque<PatOrWild> pats;
  bool relevant;
};

class MatrixRow
{
public:
  MatrixRow (PatStack pats, bool is_under_guard_)
    : pats (pats), is_under_guard_ (is_under_guard_)
  // useful (false),
  // head_is_branch (false),
  {}

  PatStack &get_pats () { return pats; }

  PatStack get_pats_clone () const { return pats; }

  const PatOrWild &head () const { return pats.head (); }
  PatOrWild &head () { return pats.head (); }

  bool is_under_guard () const { return is_under_guard_; }

  std::string to_string () const;

private:
  PatStack pats;
  bool is_under_guard_;
  // TODO: manage usefulness
};

class PlaceInfo
{
public:
  PlaceInfo (TyTy::BaseType *ty) : ty (ty) {}

  TyTy::BaseType *get_type () const { return ty; }

  std::vector<PlaceInfo> specialize (const Constructor &c) const;

private:
  TyTy::BaseType *ty;
};

class Matrix
{
public:
  Matrix (std::vector<MatrixRow> rows, std::vector<PlaceInfo> place_infos)
    : rows (rows), place_infos (place_infos)
  {}

  Matrix () {}

  std::vector<MatrixRow> &get_rows () { return rows; }

  void push_row (const MatrixRow &row) { rows.push_back (row); }

  std::vector<PlaceInfo> &get_place_infos () { return place_infos; }

  std::vector<PatOrWild> heads () const
  {
    std::vector<PatOrWild> ret;
    for (const MatrixRow &row : rows)
      ret.push_back (row.head ());

    return ret;
  }

  Matrix specialize (const Constructor &ctor) const;

  std::string to_string () const;

private:
  std::vector<MatrixRow> rows;
  std::vector<PlaceInfo> place_infos;
};

class MatchArm
{
public:
  MatchArm (DeconstructedPat pat, bool has_guard_)
    : pat (pat), has_guard_ (has_guard_)
  {}

  DeconstructedPat get_pat () const { return pat; }

  bool has_guard () const { return has_guard_; }

private:
  DeconstructedPat pat;
  bool has_guard_;
};

class WitnessPat
{
public:
  WitnessPat (Constructor ctor, std::vector<WitnessPat> fields,
	      TyTy::BaseType *ty)
    : ctor (ctor), fields (fields), ty (ty)
  {}

  static WitnessPat make_wildcard (TyTy::BaseType *ty)
  {
    return WitnessPat (Constructor::make_wildcard (), {}, ty);
  }

  const Constructor &get_ctor () const { return ctor; }

  const std::vector<WitnessPat> &get_fields () const { return fields; }

  TyTy::BaseType *get_type () const { return ty; }

  std::string to_string () const;

private:
  Constructor ctor;
  std::vector<WitnessPat> fields;
  TyTy::BaseType *ty;
};

class WitnessMatrix
{
public:
  // Create an empty witness matrix.
  static WitnessMatrix make_empty () { return WitnessMatrix ({}); }

  // Create a unit witness matrix, a new single witness.
  static WitnessMatrix make_unit ()
  {
    return WitnessMatrix ({std::vector<WitnessPat> ()});
  }

  bool empty () const { return patstacks.empty (); }

  const std::vector<std::vector<WitnessPat>> &get_stacks () const
  {
    return patstacks;
  }

  // Reverses specialization.
  void apply_constructor (const Constructor &ctor,
			  const std::set<Constructor> &missings,
			  TyTy::BaseType *ty);

  void extend (const WitnessMatrix &other);

private:
  WitnessMatrix (std::vector<std::vector<WitnessPat>> patstacks)
    : patstacks (patstacks)
  {}

  std::vector<std::vector<WitnessPat>> patstacks;
};

} // namespace Analysis
} // namespace Rust

#endif
