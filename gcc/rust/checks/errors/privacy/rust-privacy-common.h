// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#include "rust-mapping-common.h"

namespace Rust {
namespace Privacy {

/**
 * Visibility class related specifically to DefIds. This class allows defining
 * the visibility of an item with regard to a specific module.
 *
 * Items are either public throughout a crate, or restricted to a specific
 * module. Private items are simply restricted to the current module.
 */
class ModuleVisibility
{
public:
  enum Type
  {
    Unknown,
    Public,
    Restricted,
  };

  ModuleVisibility () : kind (Unknown), module_id (UNKNOWN_DEFID) {}

  static ModuleVisibility create_restricted (DefId module_id)
  {
    return ModuleVisibility (Type::Restricted, module_id);
  }

  static ModuleVisibility create_public ()
  {
    return ModuleVisibility (Type::Public, UNKNOWN_DEFID);
  }

  Type get_kind () const { return kind; }

  const DefId &get_module_id () const { return module_id; }
  DefId &get_module_id () { return module_id; }

private:
  ModuleVisibility (Type kind, DefId module_id)
    : kind (kind), module_id (module_id)
  {}

  Type kind;
  DefId module_id;
};
} // namespace Privacy
} // namespace Rust
