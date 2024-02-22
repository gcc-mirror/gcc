// Copyright (C) 2024 Free Software Foundation, Inc.

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

#include "rust-expand-format-args.h"
#include "rust-builtin-ast-nodes.h"

namespace Rust {

tl::optional<AST::Fragment>
expand_format_args (AST::FormatArgs &fmt)
{
  for (const auto &node : fmt.get_template ().get_pieces ())
    {
      switch (node.tag)
	{
	case Fmt::ffi::Piece::Tag::String:
	  // rust_debug ("[ARTHUR]: %s", node.string._0.c_str ());

	case Fmt::ffi::Piece::Tag::NextArgument:
	  rust_debug ("[ARTHUR]: NextArgument");
	  break;
	}
    }

  return tl::nullopt;
}

} // namespace Rust
