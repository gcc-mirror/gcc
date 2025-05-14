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

#ifndef RUST_BIR_BASE_H
#define RUST_BIR_BASE_H

#include "rust-bir-place.h"
#include "rust-bir-visitor.h"

#include "polonius/rust-polonius-ffi.h"
#include "rust-tyty-variance-analysis.h"

namespace Rust {

namespace BIR {

struct BasicBlock;
struct BasicBlockId;
using BasicBlocks = IndexVec<BasicBlockId, BasicBlock>;
class Statement;
class AbstractExpr;

/** Unique identifier for a basic block in the BIR. */
struct BasicBlockId
{
  uint32_t value;
  // some overloads for comparision
  bool operator== (const BasicBlockId &rhs) const { return value == rhs.value; }
  bool operator!= (const BasicBlockId &rhs) const
  {
    return !(operator== (rhs));
  }
  bool operator< (const BasicBlockId &rhs) const { return value < rhs.value; }
  bool operator> (const BasicBlockId &rhs) const { return value > rhs.value; }
  bool operator<= (const BasicBlockId &rhs) const { return !(operator> (rhs)); }
  bool operator>= (const BasicBlockId &rhs) const { return !(operator< (rhs)); }
};

static constexpr BasicBlockId INVALID_BB
  = {std::numeric_limits<uint32_t>::max ()};
static constexpr BasicBlockId ENTRY_BASIC_BLOCK = {0};

/**
 * Top-level entity of the Borrow-checker IR (BIR).
 * It represents a single function (method, closure, etc.), which is the
 * basic unit of borrow-checking.
 */
struct Function
{
  PlaceDB place_db;
  std::vector<PlaceId> arguments;
  BasicBlocks basic_blocks;
  FreeRegions universal_regions;
  std::vector<std::pair<FreeRegion, FreeRegion>> universal_region_bounds;
  std::unordered_map<Polonius::Origin, HIR::LifetimeParam *> region_hir_map;
  location_t location;
};

/** Single statement of BIR. */
class Statement
{
public:
  enum class Kind
  {
    ASSIGNMENT,		  // <place> = <expr>
    SWITCH,		  // switch <place>
    RETURN,		  // return
    GOTO,		  // goto
    STORAGE_DEAD,	  // StorageDead(<place>)
    STORAGE_LIVE,	  // StorageLive(<place>)
    USER_TYPE_ASCRIPTION, // UserTypeAscription(<place>, <tyty>)
    FAKE_READ,
  };

private:
  Kind kind;
  // ASSIGNMENT: lhs
  // SWITCH: switch_val
  // StorageDead/StorageLive: place
  // otherwise: <unused>
  PlaceId place;
  // ASSIGNMENT: rhs
  // otherwise: <unused>
  std::unique_ptr<AbstractExpr> expr;
  TyTy::BaseType *type;
  // stores location of the actual expression from source code
  // currently only available when kind is ASSIGNMENT | RETURN
  // FIXME: Add location for other statement kinds
  location_t location;

public:
  static Statement make_assignment (PlaceId place, AbstractExpr *rhs,
				    location_t location)
  {
    return Statement (Kind::ASSIGNMENT, place, rhs, nullptr, location);
  }
  static Statement make_switch (PlaceId place)
  {
    return Statement (Kind::SWITCH, place);
  }
  static Statement make_return (location_t location)
  {
    return Statement (Kind::RETURN, INVALID_PLACE, nullptr, nullptr, location);
  }
  static Statement make_goto () { return Statement (Kind::GOTO); }
  static Statement make_storage_dead (PlaceId place)
  {
    return Statement (Kind::STORAGE_DEAD, place);
  }
  static Statement make_storage_live (PlaceId place)
  {
    return Statement (Kind::STORAGE_LIVE, place);
  }
  static Statement make_user_type_ascription (PlaceId place,
					      TyTy::BaseType *type)
  {
    return Statement (Kind::USER_TYPE_ASCRIPTION, place, nullptr, type);
  }
  static Statement make_fake_read (PlaceId place)
  {
    return Statement (Kind::FAKE_READ, place);
  }

private:
  // compelete constructor, used by make_* functions
  Statement (Kind kind, PlaceId place = INVALID_PLACE,
	     AbstractExpr *rhs = nullptr, TyTy::BaseType *type = nullptr,
	     location_t location = UNKNOWN_LOCATION)
    : kind (kind), place (place), expr (rhs), type (type), location (location)
  {}

public:
  WARN_UNUSED_RESULT Kind get_kind () const { return kind; }
  WARN_UNUSED_RESULT PlaceId get_place () const { return place; }
  WARN_UNUSED_RESULT AbstractExpr &get_expr () const { return *expr; }
  WARN_UNUSED_RESULT TyTy::BaseType *get_type () const { return type; }
  WARN_UNUSED_RESULT location_t get_location () const { return location; }
};

struct BasicBlock
{
  // BIR "instructions".
  std::vector<Statement> statements;
  // A basic block can end with: goto, return or switch
  std::vector<BasicBlockId> successors;

public:
  WARN_UNUSED_RESULT bool is_terminated () const;

  WARN_UNUSED_RESULT bool is_goto_terminated () const
  {
    return is_terminated ()
	   && statements.back ().get_kind () == Statement::Kind::GOTO;
  }
};

enum class ExprKind
{
  INITIALIZER,
  OPERATOR,
  BORROW,
  ASSIGNMENT,
  CALL,
};

// Rhs expression of BIR assignment statements (abstract).
class AbstractExpr : public Visitable
{
  ExprKind kind;

public:
  explicit AbstractExpr (ExprKind kind) : kind (kind) {}
  WARN_UNUSED_RESULT ExprKind get_kind () const { return kind; }

  virtual ~AbstractExpr () {}
};

class InitializerExpr : public VisitableImpl<AbstractExpr, InitializerExpr>
{
  std::vector<PlaceId> values;

public:
  explicit InitializerExpr (std::vector<PlaceId> &&values)
    : VisitableImpl<AbstractExpr, InitializerExpr> (ExprKind::INITIALIZER),
      values (values)
  {}

public:
  std::vector<PlaceId> &get_values () { return values; }
  WARN_UNUSED_RESULT const std::vector<PlaceId> &get_values () const
  {
    return values;
  }
};

template <unsigned ARITY>
class Operator : public VisitableImpl<AbstractExpr, Operator<ARITY>>
{
  std::array<PlaceId, ARITY> operands;

public:
  explicit Operator (std::array<PlaceId, ARITY> &&operands)
    : VisitableImpl<AbstractExpr, Operator<ARITY>> (ExprKind::OPERATOR),
      operands (operands)
  {}

public:
  template <size_t I> WARN_UNUSED_RESULT PlaceId get_operand () const
  {
    static_assert (I < ARITY, "Index out of bounds");
    return operands[I];
  }
};

class BorrowExpr : public VisitableImpl<AbstractExpr, BorrowExpr>
{
  PlaceId place;
  LoanId loan;
  Polonius::Origin origin;

public:
  explicit BorrowExpr (PlaceId place, LoanId loan_id, Polonius::Origin lifetime)
    : VisitableImpl<AbstractExpr, BorrowExpr> (ExprKind::BORROW), place (place),
      loan (loan_id), origin (lifetime)
  {}
  WARN_UNUSED_RESULT PlaceId get_place () const { return place; }
  WARN_UNUSED_RESULT LoanId get_loan_id () const { return loan; }
  WARN_UNUSED_RESULT Polonius::Origin get_origin () const { return origin; }
};

/**
 * This expression is only to be used inside the assignment statement and acts
 * as identity wrapper for a place value. It is separated from `Operator<1>` to
 * render it more explicitly in the dump.
 */
class Assignment : public VisitableImpl<AbstractExpr, Assignment>
{
  PlaceId rhs;

public:
  explicit Assignment (PlaceId rhs)
    : VisitableImpl<AbstractExpr, Assignment> (ExprKind::ASSIGNMENT), rhs (rhs)
  {}

public:
  WARN_UNUSED_RESULT PlaceId get_rhs () const { return rhs; }
};

class CallExpr final : public VisitableImpl<AbstractExpr, CallExpr>
{
  std::vector<PlaceId> arguments;
  PlaceId callable;

public:
  explicit CallExpr (PlaceId callable, std::vector<PlaceId> &&arguments)
    : VisitableImpl (ExprKind::CALL), arguments (arguments), callable (callable)
  {}

public:
  WARN_UNUSED_RESULT const std::vector<PlaceId> &get_arguments () const
  {
    return arguments;
  }
  WARN_UNUSED_RESULT PlaceId get_callable () const { return callable; }
};

inline bool
BasicBlock::is_terminated () const
{
  if (statements.empty ())
    return false;
  switch (statements.back ().get_kind ())
    {
    case Statement::Kind::GOTO:
    case Statement::Kind::RETURN:
    case Statement::Kind::SWITCH:
      return true;
    case Statement::Kind::ASSIGNMENT:
      return statements.back ().get_expr ().get_kind () == ExprKind::CALL;
    default:
      return false;
    }
}

} // namespace BIR

} // namespace Rust

#endif // RUST_BIR_BASE_H
