// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#include "rust-system.h"

#ifndef BIMAP_H
#define BIMAP_H

// very simple bi-directional hashmap
template <typename K, typename V> class BiMap
{
public:
  BiMap (std::unordered_map<K, V> &&original) : map (std::move (original))
  {
    for (auto &kv : map)
      rmap.insert ({kv.second, kv.first});
  }

  const tl::optional<const V &> lookup (const K &key) const
  {
    auto itr = map.find (key);
    if (itr == map.end ())
      return tl::nullopt;

    return itr->second;
  }
  const tl::optional<const K &> lookup (const V &key) const
  {
    auto itr = rmap.find (key);
    if (itr == rmap.end ())
      return tl::nullopt;

    return itr->second;
  }

private:
  std::unordered_map<K, V> map;
  std::unordered_map<V, K> rmap;
};

#endif // !BIMAP_H
