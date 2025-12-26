// Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

#ifndef RUST_PARSE_ERROR_H
#define RUST_PARSE_ERROR_H

#include "expected.h"
#include "rust-ast.h"
#include "rust-parse-utils.h"

namespace Rust {
namespace Parse {
namespace Error {

struct Attribute
{
  static tl::expected<AST::Attribute, Attribute> make_malformed ()
  {
    return tl::unexpected<Attribute> (Attribute (Kind::MALFORMED));
  }

  static tl::expected<AST::Attribute, Attribute> make_malformed_body ()
  {
    return tl::unexpected<Attribute> (Attribute (Kind::MALFORMED_BODY));
  }

  static tl::expected<AST::Attribute, Attribute> make_unexpected_inner ()
  {
    return tl::unexpected<Attribute> (Attribute (Kind::UNEXPECTED_INNER));
  }

  enum class Kind
  {
    MALFORMED,
    MALFORMED_BODY,
    UNEXPECTED_INNER,
  } kind;

private:
  Attribute (Kind kind) : kind (kind) {}
};

struct AttributeBody
{
  static tl::expected<Parse::AttributeBody, AttributeBody> make_invalid_path ()
  {
    return tl::unexpected<AttributeBody> (AttributeBody (Kind::INVALID_PATH));
  }

  static tl::expected<Parse::AttributeBody, AttributeBody>
  make_invalid_attrinput ()
  {
    return tl::unexpected<AttributeBody> (
      AttributeBody (Kind::INVALID_ATTRINPUT));
  }

  enum class Kind
  {
    INVALID_PATH,
    INVALID_ATTRINPUT,
  } kind;

private:
  AttributeBody (Kind kind) : kind (kind) {}
};

struct SimplePathSegment
{
  static tl::expected<AST::SimplePathSegment, SimplePathSegment>
  make_invalid_token_or_path_end ()
  {
    return tl::unexpected<SimplePathSegment> (
      SimplePathSegment (Kind::INVALID_SIMPLE_PATH_TOKEN));
  }

  enum class Kind
  {
    /* Invalid token found whilst parsing a simple path segment, could be an
       error or the end of the path */
    INVALID_SIMPLE_PATH_TOKEN,
  } kind;

private:
  SimplePathSegment (Kind kind) : kind (kind) {}
};

struct PathIdentSegment
{
  static tl::expected<AST::PathIdentSegment, PathIdentSegment>
  make_invalid_token ()
  {
    return tl::unexpected<PathIdentSegment> (
      PathIdentSegment (Kind::INVALID_PATH_IDENT_TOKEN));
  }

  enum class Kind
  {
    INVALID_PATH_IDENT_TOKEN,
  } kind;

private:
  PathIdentSegment (Kind kind) : kind (kind) {}
};

struct AttrInput
{
  static tl::expected<std::unique_ptr<AST::AttrInput>, AttrInput>
  make_malformed ()
  {
    return tl::unexpected<AttrInput> (AttrInput (Kind::MALFORMED));
  }

  static tl::expected<std::unique_ptr<AST::AttrInput>, AttrInput>
  make_bad_macro_invocation ()
  {
    return tl::unexpected<AttrInput> (AttrInput (Kind::BAD_MACRO_INVOCATION));
  }

  static tl::expected<std::unique_ptr<AST::AttrInput>, AttrInput>
  make_missing_attrinput ()
  {
    return tl::unexpected<AttrInput> (AttrInput (Kind::MISSING));
  }

  static tl::expected<std::unique_ptr<AST::AttrInput>, AttrInput>
  make_bad_token_tree ()
  {
    return tl::unexpected<AttrInput> (AttrInput (Kind::BAD_TOKEN_TREE));
  }

  enum class Kind
  {
    MALFORMED,
    BAD_MACRO_INVOCATION,
    BAD_TOKEN_TREE,
    // Not an hard error in some context
    MISSING,
  } kind;

private:
  AttrInput (Kind kind) : kind (kind) {}
};

struct Item
{
  static tl::expected<std::unique_ptr<AST::Item>, Item> make_end_of_file ()
  {
    return tl::unexpected<Item> (Item (Kind::END_OF_FILE));
  }

  static tl::expected<std::unique_ptr<AST::Item>, Item> make_malformed ()
  {
    return tl::unexpected<Item> (Item (Kind::MALFORMED));
  }

  enum class Kind
  {
    END_OF_FILE,
    MALFORMED,
  } kind;

private:
  Item (Kind kind) : kind (kind) {}
};

struct Items
{
  static tl::expected<std::vector<std::unique_ptr<AST::Item>>, Items>
  make_malformed (std::vector<std::unique_ptr<AST::Item>> items)
  {
    return tl::unexpected<Items> (Items (Kind::MALFORMED, std::move (items)));
  }

  enum class Kind
  {
    MALFORMED,
  } kind;

  Items (Items const &) = delete;
  Items &operator= (Items const &) = delete;

  Items (Items &&items) = default;
  Items &operator= (Items &&) = default;

  // Should we do anything with valid items ?
  std::vector<std::unique_ptr<AST::Item>> items;

private:
  Items (Kind kind, std::vector<std::unique_ptr<AST::Item>> items)
    : kind (kind), items (std::move (items))
  {}
};

struct Visibility
{
  static tl::expected<AST::Visibility, Visibility> make_malformed ()
  {
    return tl::unexpected<Visibility> (Visibility (Kind::MALFORMED));
  }

  static tl::expected<AST::Visibility, Visibility> make_missing_path ()
  {
    return tl::unexpected<Visibility> (Visibility (Kind::MISSING_PATH));
  }

  enum class Kind
  {
    MISSING_PATH,
    MALFORMED,
  } kind;

private:
  Visibility (Kind kind) : kind (kind) {}
};

struct LifetimeParam
{
  static tl::expected<AST::LifetimeParam, LifetimeParam>
  make_not_a_lifetime_param ()
  {
    return tl::unexpected<LifetimeParam> (
      LifetimeParam (Kind::NOT_A_LIFETIME_PARAM));
  }

  enum class Kind
  {
    NOT_A_LIFETIME_PARAM,
  } kind;

private:
  LifetimeParam (Kind kind) : kind (kind) {}
};

class Lifetime
{
};

struct LoopLabel
{
  static tl::expected<AST::LoopLabel, LoopLabel> make_not_loop_label ()
  {
    return tl::unexpected<LoopLabel> (LoopLabel (Kind::NOT_LOOP_LABEL));
  }

  static tl::expected<AST::LoopLabel, LoopLabel> make_missing_colon ()
  {
    return tl::unexpected<LoopLabel> (LoopLabel (Kind::MISSING_COLON));
  }

  enum class Kind
  {
    // Not an hard error
    NOT_LOOP_LABEL,
    // Hard error
    MISSING_COLON,
  } kind;

private:
  LoopLabel (Kind kind) : kind (kind) {}
};

struct Self
{
  static tl::expected<std::unique_ptr<AST::Param>, Self>
  make_self_raw_pointer ()
  {
    return tl::unexpected<Self> (Self (Kind::SELF_RAW_PTR));
  }

  static tl::expected<std::unique_ptr<AST::Param>, Self> make_not_self ()
  {
    return tl::unexpected<Self> (Self (Kind::NOT_SELF));
  }

  static tl::expected<std::unique_ptr<AST::Param>, Self> make_parsing_error ()
  {
    return tl::unexpected<Self> (Self (Kind::PARSING));
  }

  enum class Kind
  {
    SELF_RAW_PTR,
    PARSING,
    NOT_SELF,
  } kind;

private:
  Self (Kind kind) : kind (kind) {}
};

enum class Expr
{
  MALFORMED,
  CHILD_ERROR,
  NULL_EXPR,
  NULL_DENOTATION,
  LEFT_DENOTATION,
};

enum class StructExprField
{
  MALFORMED,
  CHILD_ERROR,
  // Not a hard error
  STRUCT_BASE,
};

// Generic intermediate AST node error used when the errors need no special
// handling
enum class Node
{
  // Unexpected or missing token whilst parsing the node
  MALFORMED,
  // Error whilst parsing a child construct for the current node
  CHILD_ERROR,
};

} // namespace Error
} // namespace Parse
} // namespace Rust

#endif /* !RUST_PARSE_ERROR_H */
