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

#include "rust-system.h"

#ifndef BIMAP_H
#define BIMAP_H

// very simple bi-directional hashmap
template <typename K, typename V> class BiMap
{
  using v_iter = typename std::unordered_map<K, V>::const_iterator;
  using k_iter = typename std::unordered_map<V, K>::const_iterator;

public:
  BiMap (std::unordered_map<K, V> &&original) : map (std::move (original))
  {
    for (auto &kv : map)
      rmap.insert ({kv.second, kv.first});
  }

  const v_iter lookup (const K &key) const { return map.find (key); }
  const k_iter lookup (const V &key) const { return rmap.find (key); }

  bool is_iter_ok (const v_iter &iter) const { return iter != map.end (); }
  bool is_iter_ok (const k_iter &iter) const { return iter != rmap.end (); }

private:
  std::unordered_map<K, V> map;
  std::unordered_map<V, K> rmap;
};

#endif // !BIMAP_H
