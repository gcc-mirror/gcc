// Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

#ifndef RUST_EXPAND_FORMAT_ARGS_H
#define RUST_EXPAND_FORMAT_ARGS_H

#include "optional.h"
#include "rust-ast-fragment.h"

namespace Rust {
namespace Fmt {

tl::optional<AST::Fragment>
expand_format_args (AST::FormatArgs &fmt,
		    std::vector<std::unique_ptr<AST::Token>> &&tokens);

} // namespace Fmt
} // namespace Rust

#endif //! RUST_EXPAND_FORMAT_ARGS_H
