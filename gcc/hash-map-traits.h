/* A hash map traits.
   Copyright (C) 2015 Free Software Foundation, Inc.

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

/* implement default behavior for traits when types allow it.  */

struct default_hashmap_traits
{
  /* Hashes the passed in key.  */

  template<typename T>
  static hashval_t
  hash (T *p)
    {
      return uintptr_t (p) >> 3;
    }

  /* If the value converts to hashval_t just use it.  */

  template<typename T> static hashval_t hash (T v) { return v; }

  /* Return true if the two keys passed as arguments are equal.  */

  template<typename T>
  static bool
  equal_keys (const T &a, const T &b)
    {
      return a == b;
    }

  /* Called to dispose of the key and value before marking the entry as
     deleted.  */

  template<typename T> static void remove (T &v) { v.~T (); }

  /* Mark the passed in entry as being deleted.  */

  template<typename T>
  static void
  mark_deleted (T &e)
    {
      mark_key_deleted (e.m_key);
    }

  /* Mark the passed in entry as being empty.  */

  template<typename T>
  static void
  mark_empty (T &e)
    {
      mark_key_empty (e.m_key);
    }

  /* Return true if the passed in entry is marked as deleted.  */

  template<typename T>
  static bool
  is_deleted (T &e)
    {
      return e.m_key == (void *)1;
    }

  /* Return true if the passed in entry is marked as empty.  */

  template<typename T> static bool is_empty (T &e) { return e.m_key == NULL; }

private:
  template<typename T>
  static void
  mark_key_deleted (T *&k)
    {
      k = reinterpret_cast<T *> (1);
    }

  template<typename T>
  static void
  mark_key_empty (T *&k)
    {
      k = static_cast<T *> (0);
    }
};

#endif // HASH_MAP_TRAITS_H
