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

#ifndef RUST_EXTERN_CRATE_H
#define RUST_EXTERN_CRATE_H

#include "rust-system.h"
#include "rust-imports.h"

namespace Rust {
namespace Imports {

class ExternCrate
{
public:
  ExternCrate (Import::Stream &stream);
  ~ExternCrate ();

  bool ok () const;

  bool load (Location locus);

  const std::string &get_crate_name () const;

  const std::string &get_metadata () const;

  static bool string_to_int (Location locus, const std::string &s,
			     bool is_neg_ok, int *ret);

private:
  Import::Stream &import_stream;

  std::string crate_name;
  std::string metadata_buffer;
};

} // namespace Imports
} // namespace Rust

#endif // RUST_EXTERN_CRATE_H
