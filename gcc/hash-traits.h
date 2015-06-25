/* Traits for hashable types.
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

#ifndef hash_traits_h
#define hash_traits_h

/* Helpful type for removing with free.  */

template <typename Type>
struct typed_free_remove
{
  static inline void remove (Type *p);
};


/* Remove with free.  */

template <typename Type>
inline void
typed_free_remove <Type>::remove (Type *p)
{
  free (p);
}


/* Helpful type for a no-op remove.  */

template <typename Type>
struct typed_noop_remove
{
  static inline void remove (Type *p);
};


/* Remove doing nothing.  */

template <typename Type>
inline void
typed_noop_remove <Type>::remove (Type *p ATTRIBUTE_UNUSED)
{
}


/* Pointer hasher based on pointer equality.  Other types of pointer hash
   can inherit this and override the hash and equal functions with some
   other form of equality (such as string equality).  */

template <typename Type>
struct pointer_hash
{
  typedef Type *value_type;
  typedef Type *compare_type;

  static inline hashval_t hash (const value_type &);
  static inline bool equal (const value_type &existing,
			    const compare_type &candidate);
  static inline void mark_deleted (Type *&);
  static inline void mark_empty (Type *&);
  static inline bool is_deleted (Type *);
  static inline bool is_empty (Type *);
};

template <typename Type>
inline hashval_t
pointer_hash <Type>::hash (const value_type &candidate)
{
  /* This is a really poor hash function, but it is what the current code uses,
     so I am reusing it to avoid an additional axis in testing.  */
  return (hashval_t) ((intptr_t)candidate >> 3);
}

template <typename Type>
inline bool
pointer_hash <Type>::equal (const value_type &existing,
			   const compare_type &candidate)
{
  return existing == candidate;
}

template <typename Type>
inline void
pointer_hash <Type>::mark_deleted (Type *&e)
{
  e = reinterpret_cast<Type *> (1);
}

template <typename Type>
inline void
pointer_hash <Type>::mark_empty (Type *&e)
{
  e = NULL;
}

template <typename Type>
inline bool
pointer_hash <Type>::is_deleted (Type *e)
{
  return e == reinterpret_cast<Type *> (1);
}

template <typename Type>
inline bool
pointer_hash <Type>::is_empty (Type *e)
{
  return e == NULL;
}

/* Hasher for entry in gc memory.  */

template<typename T>
struct ggc_hasher
{
  typedef T value_type;
  typedef T compare_type;

  static void remove (T &) {}

  static void
  ggc_mx (T &p)
  {
    extern void gt_ggc_mx (T &);
    gt_ggc_mx (p);
  }

  static void
  pch_nx (T &p)
  {
    extern void gt_pch_nx (T &);
    gt_pch_nx (p);
  }

  static void
  pch_nx (T &p, gt_pointer_operator op, void *cookie)
  {
    op (&p, cookie);
  }
};

/* Hasher for cache entry in gc memory.  */

template<typename T>
struct ggc_cache_hasher : ggc_hasher<T>
{
  /* Entries are weakly held because this is for caches.  */
  static void ggc_mx (T &) {}

  static int
  keep_cache_entry (T &e)
  {
    return ggc_marked_p (e) ? -1 : 0;
  }
};

/* Traits for pointer elements that should not be freed when an element
   is deleted.  */

template <typename T>
struct nofree_ptr_hash : pointer_hash <T>, typed_noop_remove <T> {};

#endif
