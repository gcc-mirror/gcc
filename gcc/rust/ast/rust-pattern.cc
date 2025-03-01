/* General AST-related method implementations for Rust frontend.
   Copyright (C) 2009-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "rust-system.h"
#include "rust-ast-full.h"
#include "rust-diagnostics.h"
#include "rust-ast-visitor.h"
#include "rust-macro.h"
#include "rust-lex.h"
#include "rust-parse.h"
#include "rust-operators.h"

namespace Rust {
namespace AST {

RangeKind
tokenid_to_rangekind (TokenId id)
{
  switch (id)
    {
    case DOT_DOT_EQ:
      return RangeKind::INCLUDED;
    case ELLIPSIS:
      return RangeKind::ELLIPSIS;
    case DOT_DOT:
      return RangeKind::EXCLUDED;
    default:
      rust_unreachable ();
    }
}

std::string
LiteralPattern::as_string () const
{
  return lit.as_string ();
}

std::string
IdentifierPattern::as_string () const
{
  // TODO: maybe rewrite to work with non-linearisable patterns
  std::string str;

  if (is_ref)
    str += "ref ";

  if (is_mut)
    str += "mut ";

  str += variable_ident.as_string ();

  if (has_pattern_to_bind ())
    str += " @ " + to_bind->as_string ();

  return str;
}

std::string
RangePatternBoundLiteral::as_string () const
{
  std::string str;

  if (has_minus)
    str += "-";

  str += literal.as_string ();

  return str;
}

std::string
RangePattern::as_string () const
{
  // TODO: maybe rewrite to work with non-linearisable bounds
  switch (range_kind)
    {
    case RangeKind::EXCLUDED:
      return lower->as_string () + ".." + upper->as_string ();
    case RangeKind::INCLUDED:
      return lower->as_string () + "..=" + upper->as_string ();
    case RangeKind::ELLIPSIS:
      return lower->as_string () + "..." + upper->as_string ();
    default:
      rust_unreachable ();
    }
}

std::string
ReferencePattern::as_string () const
{
  // TODO: maybe rewrite to work with non-linearisable patterns
  std::string str ("&");

  if (has_two_amps)
    str += "&";

  if (is_mut)
    str += "mut ";

  str += pattern->as_string ();

  return str;
}

std::string
StructPatternField::as_string () const
{
  // outer attributes
  std::string str = append_attributes (outer_attrs, OUTER);

  return str;
}

std::string
StructPatternFieldTuplePat::as_string () const
{
  // TODO: maybe rewrite to work with non-linearisable patterns
  std::string str = StructPatternField::as_string ();

  str += "\n";

  str += std::to_string (index) + " : " + tuple_pattern->as_string ();

  return str;
}

std::string
StructPatternFieldIdentPat::as_string () const
{
  // TODO: maybe rewrite to work with non-linearisable patterns
  std::string str = StructPatternField::as_string ();

  str += "\n";

  str += ident.as_string () + " : " + ident_pattern->as_string ();

  return str;
}

std::string
StructPatternFieldIdent::as_string () const
{
  std::string str = StructPatternField::as_string ();

  str += "\n";

  if (has_ref)
    str += "ref ";

  if (has_mut)
    str += "mut ";

  str += ident.as_string ();

  return str;
}

std::string
StructPatternElements::as_string () const
{
  std::string str ("\n  Fields: ");

  if (!has_struct_pattern_fields ())
    {
      str += "none";
    }
  else
    {
      for (const auto &field : fields)
	str += "\n   " + field->as_string ();
    }

  str += "\n  Etc: ";
  if (has_struct_pattern_etc)
    str += "true";
  else
    str += "false";

  return str;
}

std::string
StructPattern::as_string () const
{
  std::string str ("StructPattern: \n Path: ");

  str += path.as_string ();

  str += "\n Struct pattern elems: ";
  if (!has_struct_pattern_elems ())
    str += "none";
  else
    str += elems.as_string ();

  return str;
}

std::string
TupleStructItemsNoRange::as_string () const
{
  std::string str;

  for (const auto &pattern : patterns)
    str += "\n  " + pattern->as_string ();

  return str;
}

std::string
TupleStructItemsRange::as_string () const
{
  std::string str ("\n  Lower patterns: ");

  if (lower_patterns.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &lower : lower_patterns)
	str += "\n   " + lower->as_string ();
    }

  str += "\n  Upper patterns: ";
  if (upper_patterns.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &upper : upper_patterns)
	str += "\n   " + upper->as_string ();
    }

  return str;
}

std::string
TupleStructPattern::as_string () const
{
  std::string str ("TupleStructPattern: \n Path: ");

  str += path.as_string ();

  str += "\n Tuple struct items: " + items->as_string ();

  return str;
}

std::string
TuplePatternItemsMultiple::as_string () const
{
  std::string str;

  for (const auto &pattern : patterns)
    str += "\n " + pattern->as_string ();

  return str;
}

std::string
TuplePatternItemsRanged::as_string () const
{
  std::string str;

  str += "\n Lower patterns: ";
  if (lower_patterns.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &lower : lower_patterns)
	str += "\n  " + lower->as_string ();
    }

  str += "\n Upper patterns: ";
  if (upper_patterns.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &upper : upper_patterns)
	str += "\n  " + upper->as_string ();
    }

  return str;
}

std::string
TuplePattern::as_string () const
{
  return "TuplePattern: " + items->as_string ();
}

std::string
GroupedExpr::as_string () const
{
  std::string str ("Grouped expr:");

  // outer attrs
  str += append_attributes (outer_attrs, OUTER);

  // inner attributes
  str += append_attributes (inner_attrs, INNER);

  str += "\n Expr in parens: " + expr_in_parens->as_string ();

  return str;
}

std::string
SlicePattern::as_string () const
{
  std::string str ("SlicePattern: ");

  for (const auto &pattern : items)
    str += "\n " + pattern->as_string ();

  return str;
}

std::string
AltPattern::as_string () const
{
  std::string str ("AltPattern: ");

  for (const auto &pattern : alts)
    str += "\n " + pattern->as_string ();

  return str;
}

void
AltPattern::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
GroupedPattern::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
GroupedExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
SlicePattern::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TuplePatternItemsRanged::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TuplePattern::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TuplePatternItemsMultiple::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
LiteralPattern::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
IdentifierPattern::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
WildcardPattern::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
RestPattern::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
RangePatternBoundLiteral::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
RangePatternBoundPath::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
RangePatternBoundQualPath::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
RangePattern::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ReferencePattern::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
StructPatternFieldTuplePat::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
StructPatternFieldIdentPat::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
StructPatternFieldIdent::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
StructPattern::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TupleStructItemsNoRange::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TupleStructItemsRange::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TupleStructPattern::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

} // namespace AST
} // namespace Rust
