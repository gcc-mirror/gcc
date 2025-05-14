// Copyright (C) 2025 Free Software Foundation, Inc.

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

#include <ast/rust-ast-full-decls.h>

namespace Rust {

/*
 * Used to convert different path segment object references
 * into SimplePathSegment/PathIdentSegment references
 *
 * unwrap_type_segment:
 *   expands to a call to unwrap_type_segment_inner::unwrap,
 *   used for type inference
 */
#define unwrap_type_segment(x)                                                 \
  (unwrap_type_segment_inner<typename std::remove_const<                       \
     typename std::remove_reference<decltype (x)>::type>::type>::unwrap (x))

template <class T> class unwrap_type_segment_inner;

/* base case */
template <> class unwrap_type_segment_inner<AST::SimplePathSegment>
{
public:
  /* The return type of unwrap */
  using ret = AST::SimplePathSegment;

  /* non-const qualified unwrap */
  static AST::SimplePathSegment &unwrap (AST::SimplePathSegment &x)
  {
    return x;
  }

  /* const qualified unwrap */
  static const AST::SimplePathSegment &unwrap (const AST::SimplePathSegment &x)
  {
    return x;
  }
};

/* case which dereferences unique_ptr */
template <class T> class unwrap_type_segment_inner<std::unique_ptr<T>>
{
public:
  using ret = typename unwrap_type_segment_inner<T>::ret;

  static ret &unwrap (std::unique_ptr<T> &x)
  {
    return unwrap_type_segment (*x);
  }
  static const ret &unwrap (const std::unique_ptr<T> &x)
  {
    return unwrap_type_segment (*x);
  }
};

/* case which handles objects with a get_ident_segment member function */
template <class T> class unwrap_type_segment_inner
{
public:
  using ret = AST::PathIdentSegment;

  static ret &unwrap (T &x) { return x.get_ident_segment (); }
  static const ret &unwrap (const T &x) { return x.get_ident_segment (); }
};

/*
 * Used to get the node id of a path segment object
 */
NodeId
unwrap_segment_node_id (const AST::TypePathSegment &seg);

NodeId
unwrap_segment_node_id (const AST::SimplePathSegment &seg);

NodeId
unwrap_segment_node_id (const AST::PathExprSegment &seg);

template <class T>
NodeId
unwrap_segment_node_id (const std::unique_ptr<T> &ptr)
{
  return unwrap_segment_node_id (*ptr);
}

/**
 * Used to check if a path segment is associated with a lang item
 */
tl::optional<LangItem::Kind>
unwrap_segment_get_lang_item (const AST::TypePathSegment &seg);

tl::optional<LangItem::Kind>
unwrap_segment_get_lang_item (const AST::SimplePathSegment &seg);

tl::optional<LangItem::Kind>
unwrap_segment_get_lang_item (const AST::PathExprSegment &seg);

template <class T>
tl::optional<LangItem::Kind>
unwrap_segment_get_lang_item (const std::unique_ptr<T> &ptr)
{
  return unwrap_segment_get_lang_item (*ptr);
}

} // namespace Rust
