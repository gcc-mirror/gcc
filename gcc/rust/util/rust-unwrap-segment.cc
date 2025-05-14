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

#include "ast/rust-path.h"

namespace Rust {

NodeId
unwrap_segment_node_id (const AST::TypePathSegment &seg)
{
  return seg.get_node_id ();
}

NodeId
unwrap_segment_node_id (const AST::SimplePathSegment &seg)
{
  return seg.get_node_id ();
}

NodeId
unwrap_segment_node_id (const AST::PathExprSegment &seg)
{
  return seg.get_node_id ();
}

tl::optional<LangItem::Kind>
unwrap_segment_get_lang_item (const AST::TypePathSegment &seg)
{
  if (seg.is_lang_item ())
    return seg.get_lang_item ();
  return tl::nullopt;
}

tl::optional<LangItem::Kind>
unwrap_segment_get_lang_item (const AST::SimplePathSegment &seg)
{
  return tl::nullopt;
}

tl::optional<LangItem::Kind>
unwrap_segment_get_lang_item (const AST::PathExprSegment &seg)
{
  return tl::nullopt;
}

} // namespace Rust
