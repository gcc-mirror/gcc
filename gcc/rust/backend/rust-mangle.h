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

#ifndef RUST_MANGLE_H
#define RUST_MANGLE_H

#include "rust-system.h"
#include "rust-tyty.h"

namespace Rust {
namespace Compile {

class Context;

class Mangler
{
public:
  enum MangleVersion
  {
    // Values defined in rust/lang.opt
    LEGACY = 0,
    V0 = 1,
  };

  // this needs to support Legacy and V0 see github #429 or #305
  std::string mangle_item (Rust::Compile::Context *ctx,
			   const TyTy::BaseType *ty,
			   const Resolver::CanonicalPath &path) const;

  static void set_mangling (int frust_mangling_value)
  {
    version = static_cast<MangleVersion> (frust_mangling_value);
  }

private:
  static enum MangleVersion version;
};

std::string
legacy_mangle_item (const TyTy::BaseType *ty,
		    const Resolver::CanonicalPath &path);

std::string
v0_mangle_item (Rust::Compile::Context *ctx, const TyTy::BaseType *ty,
		const Resolver::CanonicalPath &path);

} // namespace Compile
} // namespace Rust
#endif // RUST_MANGLE_H
