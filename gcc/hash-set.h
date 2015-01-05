/* A type-safe hash set.
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


#ifndef hash_set_h
#define hash_set_h

#include "hash-table.h"

/* implement default behavior for traits when types allow it.  */

struct default_hashset_traits
{
  /* Hashes the passed in key.  */

  template<typename T>
  static hashval_t
  hash (T *p)
    {
      return uintptr_t(p) >> 3;
    }

  template<typename T> static hashval_t hash(const T &v) { return v; }

  /* Return true if the two keys passed as arguments are equal.  */

  template<typename T>
  static bool
  equal (const T &a, const T &b)
    {
      return a == b;
    }

  /* Called to dispose of the key before marking the entry as deleted.  */

  template<typename T> static void remove (T &v) { v.~T (); }

  /* Mark the passed in entry as being deleted.  */

  template<typename T>
  static void
  mark_deleted (T *&e)
    {
      e = reinterpret_cast<void *> (1);
    }

  /* Mark the passed in entry as being empty.  */

  template<typename T>
  static void
  mark_empty (T *&e)
    {
      e = NULL;
    }

  /* Return true if the passed in entry is marked as deleted.  */

  template<typename T>
  static bool
  is_deleted (T *e)
    {
      return e == reinterpret_cast<void *> (1);
    }

  /* Return true if the passed in entry is marked as empty.  */

  template<typename T> static bool is_empty (T *e) { return e == NULL; }

  /* ggc walking routine, mark all objects refered to by this one.  */

  template<typename T>
  static void
  ggc_mx (T &x)
    {
      extern void gt_ggc_mx (T &);
      gt_ggc_mx (x);
    }

  /* pch walking routine, note all objects refered to by this element.  */

  template<typename T>
  static void
  pch_nx (T &x)
    {
      extern void gt_pch_nx (T &);
      gt_pch_nx (x);
    }
};

template<typename Key, typename Traits = default_hashset_traits>
class hash_set
{
  struct hash_entry
  {
    Key m_key;

    typedef hash_entry value_type;
    typedef Key compare_type;
    typedef int store_values_directly;

    static hashval_t hash (const hash_entry &e)
      {
       	return Traits::hash (e.m_key);
      }

    static bool equal (const hash_entry &a, const Key &b)
       	{
	  return Traits::equal (a.m_key, b);
       	}

    static void remove (hash_entry &e) { Traits::remove (e.m_key); }

    static void
    mark_deleted (hash_entry &e)
      {
       	Traits::mark_deleted (e.m_key);
      }

    static bool is_deleted (const hash_entry &e)
      {
       	return Traits::is_deleted (e.m_key);
      }

    static void
    mark_empty (hash_entry &e)
      {
	Traits::mark_empty (e.m_key);
      }

    static bool
    is_empty (const hash_entry &e)
      {
	return Traits::is_empty (e.m_key);
      }

    static void ggc_mx (hash_entry &e)
      {
	Traits::ggc_mx (e.m_key);
      }

    static void pch_nx (hash_entry &e)
      {
	Traits::pch_nx (e.m_key);
      }

    static void pch_nx (hash_entry &e, gt_pointer_operator op, void *c)
      {
	pch_nx_helper (e.m_key, op, c);
      }

  private:
    template<typename T>
    static void
      pch_nx_helper (T &x, gt_pointer_operator op, void *cookie)
	{
	  gt_pch_nx (&x, op, cookie);
	}

    template<typename T>
      static void
      pch_nx_helper (T *&x, gt_pointer_operator op, void *cookie)
	{
	  op (&x, cookie);
	}
  };

public:
  explicit hash_set (size_t n = 13, bool ggc = false) : m_table (n, ggc) {}

  /* Create a hash_set in gc memory with space for at least n elements.  */

  static hash_set *
    create_ggc (size_t n)
      {
	hash_set *set = ggc_alloc<hash_set> ();
	new (set) hash_set (n, true);
	return set;
      }

  /* If key k isn't already in the map add it to the map, and
     return false.  Otherwise return true.  */

  bool add (const Key &k)
    {
      hash_entry *e = m_table.find_slot_with_hash (k, Traits::hash (k),
						   INSERT);
      bool existed = !hash_entry::is_empty (*e);
      if (!existed)
	e->m_key = k;

      return existed;
    }

  /* if the passed in key is in the map return its value otherwise NULL.  */

  bool contains (const Key &k)
    {
      hash_entry &e = m_table.find_with_hash (k, Traits::hash (k));
      return !Traits::is_empty (e.m_key);
    }

  /* Call the call back on each pair of key and value with the passed in
     arg.  */

  template<typename Arg, bool (*f)(const Key &, Arg)>
  void traverse (Arg a) const
    {
      for (typename hash_table<hash_entry>::iterator iter = m_table.begin ();
	   iter != m_table.end (); ++iter)
	f ((*iter).m_key, a);
    }

  /* Return the number of elements in the set.  */

  size_t elements () const { return m_table.elements (); }

private:

  template<typename T, typename U> friend void gt_ggc_mx (hash_set<T, U> *);
  template<typename T, typename U> friend void gt_pch_nx (hash_set<T, U> *);
      template<typename T, typename U> friend void gt_pch_nx (hash_set<T, U> *, gt_pointer_operator, void *);

  hash_table<hash_entry> m_table;
};

/* ggc marking routines.  */

template<typename K, typename H>
static inline void
gt_ggc_mx (hash_set<K, H> *h)
{
  gt_ggc_mx (&h->m_table);
}

template<typename K, typename H>
static inline void
gt_pch_nx (hash_set<K, H> *h)
{
  gt_pch_nx (&h->m_table);
}

template<typename K, typename H>
static inline void
gt_pch_nx (hash_set<K, H> *h, gt_pointer_operator op, void *cookie)
{
  op (&h->m_table.m_entries, cookie);
}

#endif
