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

#ifndef RUST_MAPPING_COMMON
#define RUST_MAPPING_COMMON

#include "rust-system.h"

namespace Rust {

// refers to a Crate
typedef uint32_t CrateNum;
// refers to any node in the AST in current Crate
typedef uint32_t NodeId;
// refers to any node in the HIR for the current crate
typedef uint32_t HirId;
// refers to any top-level decl in HIR
typedef uint32_t LocalDefId;

struct DefId
{
  CrateNum crateNum;
  LocalDefId localDefId;

  bool operator== (const DefId &other) const
  {
    return this->crateNum == other.crateNum
	   && this->localDefId == other.localDefId;
  }

  bool operator!= (const DefId &other) const { return !(*this == other); }

  bool operator< (const DefId &other) const
  {
    return ((uint64_t) this->crateNum << 32 | this->localDefId)
	   < ((uint64_t) other.crateNum << 32 | other.localDefId);
  }

  std::string as_string () const
  {
    std::string buf;
    buf += std::to_string (crateNum);
    buf += " "; // or anything else
    buf += std::to_string (localDefId);
    return buf;
  }
};

#define UNKNOWN_CRATENUM ((uint32_t) (UINT32_MAX))
#define UNKNOWN_NODEID ((uint32_t) (UINT32_MAX))
#define UNKNOWN_HIRID ((uint32_t) (UINT32_MAX))
#define UNKNOWN_LOCAL_DEFID ((uint32_t) (0))
#define UNKNOWN_DEFID (DefId{0, 0})

} // namespace Rust

namespace std {
template <> struct hash<Rust::DefId>
{
  size_t operator() (const Rust::DefId &id) const noexcept
  {
    // TODO: Check if we can improve performance by having a better hash
    // algorithm for `DefId`s
    return hash<uint32_t> () (hash<uint32_t> () (id.crateNum)
			      + hash<uint32_t> () (id.localDefId));
  }
};
} // namespace std

#endif // RUST_MAPPING_COMMON
