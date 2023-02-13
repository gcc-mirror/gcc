// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "rust-macro.h"

namespace Rust {
namespace AST {

BuiltinMacro
builtin_macro_from_string (const std::string &identifier)
{
  if (identifier == "assert")
    return BuiltinMacro::Assert;

  if (identifier == "file")
    return BuiltinMacro::File;

  if (identifier == "line")
    return BuiltinMacro::Line;

  if (identifier == "column")
    return BuiltinMacro::Column;

  if (identifier == "include_bytes")
    return BuiltinMacro::IncludeBytes;

  if (identifier == "include_str")
    return BuiltinMacro::IncludeStr;

  if (identifier == "compile_error")
    return BuiltinMacro::CompileError;

  if (identifier == "concat")
    return BuiltinMacro::Concat;

  if (identifier == "env")
    return BuiltinMacro::Env;

  if (identifier == "cfg")
    return BuiltinMacro::Cfg;

  if (identifier == "include")
    return BuiltinMacro::Include;

  gcc_unreachable ();
}

} // namespace AST
} // namespace Rust
