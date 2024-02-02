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

#ifndef RUST_BIR_BASE_H
#define RUST_BIR_BASE_H

#include "rust-bir-place.h"
#include "rust-bir-visitor.h"

namespace Rust {

namespace BIR {

struct BasicBlock;
class Statement;
class AbstractExpr;

/**
 * Top-level entity of the Borrow-checker IR (BIR).
 * It represents a single function (method, closure, etc.), which is the
 * basic unit of borrow-checking.
 */
struct Function
{
  PlaceDB place_db;
  std::vector<PlaceId> arguments;
  std::vector<BasicBlock> basic_blocks;
};

/** Single statement of BIR. */
class Statement
{
public:
  enum class Kind
  {
    ASSIGNMENT,	  // <place> = <expr>
    SWITCH,	  // switch <place>
    RETURN,	  // return
    GOTO,	  // goto
    STORAGE_DEAD, // StorageDead(<place>)
    STORAGE_LIVE, // StorageLive(<place>)
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

public:
  Statement (PlaceId lhs, AbstractExpr *rhs)
    : kind (Kind::ASSIGNMENT), place (lhs), expr (rhs)
  {}

  explicit Statement (Kind kind, PlaceId place = INVALID_PLACE,
		      AbstractExpr *expr = nullptr)
    : kind (kind), place (place), expr (expr)
  {}

public:
  WARN_UNUSED_RESULT Kind get_kind () const { return kind; }
  WARN_UNUSED_RESULT PlaceId get_place () const { return place; }
  WARN_UNUSED_RESULT AbstractExpr &get_expr () const { return *expr; }
};

/** Unique identifier for a basic block in the BIR. */
using BasicBlockId = uint32_t;

static constexpr BasicBlockId INVALID_BB
  = std::numeric_limits<BasicBlockId>::max ();

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
  [[nodiscard]] ExprKind get_kind () const { return kind; }
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

public:
  explicit BorrowExpr (PlaceId place)
    : VisitableImpl<AbstractExpr, BorrowExpr> (ExprKind::BORROW), place (place)
  {}
  WARN_UNUSED_RESULT PlaceId get_place () const { return place; }
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

class CallExpr : public VisitableImpl<AbstractExpr, CallExpr>
{
  std::vector<PlaceId> arguments;
  PlaceId callable;

public:
  explicit CallExpr (PlaceId callable, std::vector<PlaceId> &&arguments)
    : VisitableImpl<AbstractExpr, CallExpr> (ExprKind::CALL),
      arguments (arguments), callable (callable)
  {}

public:
  const std::vector<PlaceId> &get_arguments () { return arguments; }
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
