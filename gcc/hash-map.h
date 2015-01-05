/* A type-safe hash map.
   Copyright (C) 2014-2015 Free Software Foundation, Inc.

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


#ifndef hash_map_h
#define hash_map_h

#include <new>
#include <utility>
#include "hash-table.h"

/* implement default behavior for traits when types allow it.  */

struct default_hashmap_traits
{
  /* Hashes the passed in key.  */

  template<typename T>
  static hashval_t
  hash (T *p)
    {
      return uintptr_t(p) >> 3;
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

template<typename Key, typename Value,
	 typename Traits = default_hashmap_traits>
class GTY((user)) hash_map
{
  struct hash_entry
  {
    Key m_key;
    Value m_value;

    typedef hash_entry value_type;
    typedef Key compare_type;
    typedef int store_values_directly;

    static hashval_t hash (const hash_entry &e)
      {
       	return Traits::hash (e.m_key);
      }

    static bool equal (const hash_entry &a, const Key &b)
       	{
	  return Traits::equal_keys (a.m_key, b);
       	}

    static void remove (hash_entry &e) { Traits::remove (e); }

    static void mark_deleted (hash_entry &e) { Traits::mark_deleted (e); }

    static bool is_deleted (const hash_entry &e)
      {
       	return Traits::is_deleted (e);
      }

    static void mark_empty (hash_entry &e) { Traits::mark_empty (e); }
    static bool is_empty (const hash_entry &e) { return Traits::is_empty (e); }

    static void ggc_mx (hash_entry &e)
      {
	gt_ggc_mx (e.m_key);
	gt_ggc_mx (e.m_value);
      }

    static void pch_nx (hash_entry &e)
      {
	gt_pch_nx (e.m_key);
	gt_pch_nx (e.m_value);
      }

    static void pch_nx (hash_entry &e, gt_pointer_operator op, void *c)
      {
	pch_nx_helper (e.m_key, op, c);
	pch_nx_helper (e.m_value, op, c);
      }

  private:
    template<typename T>
    static void
      pch_nx_helper (T &x, gt_pointer_operator op, void *cookie)
	{
	  gt_pch_nx (&x, op, cookie);
	}

    static void
      pch_nx_helper (int, gt_pointer_operator, void *)
	{
	}

    static void
      pch_nx_helper (unsigned int, gt_pointer_operator, void *)
	{
	}

    static void
      pch_nx_helper (bool, gt_pointer_operator, void *)
	{
	}

    template<typename T>
      static void
      pch_nx_helper (T *&x, gt_pointer_operator op, void *cookie)
	{
	  op (&x, cookie);
	}
  };

public:
  explicit hash_map (size_t n = 13, bool ggc = false) : m_table (n, ggc) {}

  /* Create a hash_map in ggc memory.  */
  static hash_map *create_ggc (size_t size)
    {
      hash_map *map = ggc_alloc<hash_map> ();
      new (map) hash_map (size, true);
      return map;
    }

  /* If key k isn't already in the map add key k with value v to the map, and
     return false.  Otherwise set the value of the entry for key k to be v and
     return true.  */

  bool put (const Key &k, const Value &v)
    {
      hash_entry *e = m_table.find_slot_with_hash (k, Traits::hash (k),
						   INSERT);
      bool existed = !hash_entry::is_empty (*e);
      if (!existed)
	e->m_key = k;

      e->m_value = v;
      return existed;
    }

  /* if the passed in key is in the map return its value otherwise NULL.  */

  Value *get (const Key &k)
    {
      hash_entry &e = m_table.find_with_hash (k, Traits::hash (k));
      return Traits::is_empty (e) ? NULL : &e.m_value;
    }

  /* Return a reference to the value for the passed in key, creating the entry
     if it doesn't already exist.  If existed is not NULL then it is set to false
     if the key was not previously in the map, and true otherwise.  */

  Value &get_or_insert (const Key &k, bool *existed = NULL)
    {
      hash_entry *e = m_table.find_slot_with_hash (k, Traits::hash (k),
						   INSERT);
      bool ins = Traits::is_empty (*e);
      if (ins)
	e->m_key = k;

      if (existed != NULL)
	*existed = !ins;

      return e->m_value;
    }

  void remove (const Key &k)
    {
      m_table.remove_elt_with_hash (k, Traits::hash (k));
    }

  /* Call the call back on each pair of key and value with the passed in
     arg.  */

  template<typename Arg, bool (*f)(const Key &, const Value &, Arg)>
  void traverse (Arg a) const
    {
      for (typename hash_table<hash_entry>::iterator iter = m_table.begin ();
	   iter != m_table.end (); ++iter)
	f ((*iter).m_key, (*iter).m_value, a);
    }

  template<typename Arg, bool (*f)(const Key &, Value *, Arg)>
  void traverse (Arg a) const
    {
      for (typename hash_table<hash_entry>::iterator iter = m_table.begin ();
	   iter != m_table.end (); ++iter)
	if (!f ((*iter).m_key, &(*iter).m_value, a))
	  break;
    }

  size_t elements () const { return m_table.elements (); }

  class iterator
  {
  public:
    explicit iterator (const typename hash_table<hash_entry>::iterator &iter) :
      m_iter (iter) {}

    iterator &operator++ ()
    {
      ++m_iter;
      return *this;
    }

    std::pair<Key, Value> operator* ()
    {
      hash_entry &e = *m_iter;
      return std::pair<Key, Value> (e.m_key, e.m_value);
    }

    bool
    operator != (const iterator &other) const
    {
      return m_iter != other.m_iter;
    }

  private:
    typename hash_table<hash_entry>::iterator m_iter;
  };

  /* Standard iterator retrieval methods.  */

  iterator  begin () const { return iterator (m_table.begin ()); }
  iterator end () const { return iterator (m_table.end ()); }

private:

  template<typename T, typename U, typename V> friend void gt_ggc_mx (hash_map<T, U, V> *);
  template<typename T, typename U, typename V> friend void gt_pch_nx (hash_map<T, U, V> *);
      template<typename T, typename U, typename V> friend void gt_pch_nx (hash_map<T, U, V> *, gt_pointer_operator, void *);

  hash_table<hash_entry> m_table;
};

/* ggc marking routines.  */

template<typename K, typename V, typename H>
static inline void
gt_ggc_mx (hash_map<K, V, H> *h)
{
  gt_ggc_mx (&h->m_table);
}

template<typename K, typename V, typename H>
static inline void
gt_pch_nx (hash_map<K, V, H> *h)
{
  gt_pch_nx (&h->m_table);
}

template<typename K, typename V, typename H>
static inline void
gt_pch_nx (hash_map<K, V, H> *h, gt_pointer_operator op, void *cookie)
{
  op (&h->m_table.m_entries, cookie);
}

#endif
