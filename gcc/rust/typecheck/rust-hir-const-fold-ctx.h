// Copyright (C) 2020 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_CONST_FOLD_CTX_H
#define RUST_HIR_CONST_FOLD_CTX_H

#include "rust-backend.h"
#include "rust-hir-map.h"

namespace Rust {
namespace ConstFold {

class Context
{
public:
  ~Context () {}

  static void init (::Backend *backend);

  static Context *get ();

  ::Backend *get_backend () { return backend; }

  bool lookup_const (HirId id, Bexpression **expr);

  void insert_const (HirId, Bexpression *expr);

private:
  Context (::Backend *backend);

  ::Backend *backend;
  std::map<HirId, Bexpression *> ctx;
};

} // namespace ConstFold
} // namespace Rust

#endif // RUST_HIR_CONST_FOLD_CTX_H
