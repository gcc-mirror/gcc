/* A hash map traits.
   Copyright (C) 2015-2022 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef HASH_MAP_TRAITS_H
#define HASH_MAP_TRAITS_H

/* Bacause mem-stats.h uses default hashmap traits, we have to
   put the class to this separate header file.  */

#include "hash-traits.h"

/* Implement hash_map traits for a key with hash traits H.  Empty and
   deleted map entries are represented as empty and deleted keys.  */

template <typename H, typename Value>
struct simple_hashmap_traits
{
  typedef typename H::value_type key_type;
  static const bool maybe_mx = true;
  static inline hashval_t hash (const key_type &);
  static inline bool equal_keys (const key_type &, const key_type &);
  template <typename T> static inline void remove (T &);
  static const bool empty_zero_p = H::empty_zero_p;
  template <typename T> static inline bool is_empty (const T &);
  template <typename T> static inline bool is_deleted (const T &);
  template <typename T> static inline void mark_empty (T &);
  template <typename T> static inline void mark_deleted (T &);
};

template <typename H, typename Value>
inline hashval_t
simple_hashmap_traits <H, Value>::hash (const key_type &h)
{
  return H::hash (h);
}

template <typename H, typename Value>
inline bool
simple_hashmap_traits <H, Value>::equal_keys (const key_type &k1,
					      const key_type &k2)
{
  return H::equal (k1, k2);
}

template <typename H, typename Value>
template <typename T>
inline void
simple_hashmap_traits <H, Value>::remove (T &entry)
{
  H::remove (entry.m_key);
  entry.m_value.~Value ();
}

template <typename H, typename Value>
template <typename T>
inline bool
simple_hashmap_traits <H, Value>::is_empty (const T &entry)
{
  return H::is_empty (entry.m_key);
}

template <typename H, typename Value>
template <typename T>
inline bool
simple_hashmap_traits <H, Value>::is_deleted (const T &entry)
{
  return H::is_deleted (entry.m_key);
}

template <typename H, typename Value>
template <typename T>
inline void
simple_hashmap_traits <H, Value>::mark_empty (T &entry)
{
  H::mark_empty (entry.m_key);
}

template <typename H, typename Value>
template <typename T>
inline void
simple_hashmap_traits <H, Value>::mark_deleted (T &entry)
{
  H::mark_deleted (entry.m_key);
}

template <typename H, typename Value>
struct simple_cache_map_traits: public simple_hashmap_traits<H,Value>
{
  static const bool maybe_mx = false;
};

/* Implement traits for a hash_map with keys of type Key and values of
   type Value for cases in which the key cannot represent empty and
   deleted slots.  Instead record empty and deleted entries in Value.  */

template <typename Key, typename Value>
struct unbounded_hashmap_traits
{
  typedef typename Key::value_type key_type;

  static hashval_t hash (const typename Key::value_type &);
  static bool equal_keys (const typename Key::value_type &,
			  const typename Key::compare_type &);

  template <typename T> static inline void remove (T &);
  static const bool empty_zero_p = default_hash_traits <Value>::empty_zero_p;
  template <typename T> static inline bool is_empty (const T &);
  template <typename T> static inline bool is_deleted (const T &);
  template <typename T> static inline void mark_empty (T &);
  template <typename T> static inline void mark_deleted (T &);
};

template <typename Key, typename Value>
inline hashval_t
unbounded_hashmap_traits <Key, Value>
::hash (const typename Key::value_type &key)
{
  return Key::hash (key);
}

template <typename Key, typename Value>
inline bool
unbounded_hashmap_traits <Key, Value>
::equal_keys (const typename Key::value_type &x,
	      const typename Key::compare_type &y)
{
  return Key::equal (x, y);
}

template <typename Key, typename Value>
template <typename T>
inline void
unbounded_hashmap_traits <Key, Value>::remove (T &entry)
{
  default_hash_traits <Value>::remove (entry.m_value);
}

template <typename Key, typename Value>
template <typename T>
inline bool
unbounded_hashmap_traits <Key, Value>::is_empty (const T &entry)
{
  return default_hash_traits <Value>::is_empty (entry.m_value);
}

template <typename Key, typename Value>
template <typename T>
inline bool
unbounded_hashmap_traits <Key, Value>::is_deleted (const T &entry)
{
  return default_hash_traits <Value>::is_deleted (entry.m_value);
}

template <typename Key, typename Value>
template <typename T>
inline void
unbounded_hashmap_traits <Key, Value>::mark_empty (T &entry)
{
  default_hash_traits <Value>::mark_empty (entry.m_value);
}

template <typename Key, typename Value>
template <typename T>
inline void
unbounded_hashmap_traits <Key, Value>::mark_deleted (T &entry)
{
  default_hash_traits <Value>::mark_deleted (entry.m_value);
}

/* Implement traits for a hash_map from integer type Key to Value in
   cases where Key has no spare values for recording empty and deleted
   slots.  */

template <typename Key, typename Value>
using unbounded_int_hashmap_traits
  = unbounded_hashmap_traits <int_hash_base <Key>, Value>;

#endif // HASH_MAP_TRAITS_H
