/* A type-safe hash table template.
   Copyright (C) 2012-2013 Free Software Foundation, Inc.
   Contributed by Lawrence Crowl <crowl@google.com>

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


/* This file implements a typed hash table.
   The implementation borrows from libiberty's htab_t in hashtab.h.


   INTRODUCTION TO TYPES

   Users of the hash table generally need to be aware of three types.

      1. The type being placed into the hash table.  This type is called
      the value type.

      2. The type used to describe how to handle the value type within
      the hash table.  This descriptor type provides the hash table with
      several things.

         - A typedef named 'value_type' to the value type (from above).

         - A static member function named 'hash' that takes a value_type
         pointer and returns a hashval_t value.

         - A typedef named 'compare_type' that is used to test when an value
         is found.  This type is the comparison type.  Usually, it will be the
         same as value_type.  If it is not the same type, you must generally
         explicitly compute hash values and pass them to the hash table.

         - A static member function named 'equal' that takes a value_type
         pointer and a compare_type pointer, and returns a bool.

         - A static function named 'remove' that takes an value_type pointer
         and frees the memory allocated by it.  This function is used when
         individual elements of the table need to be disposed of (e.g.,
         when deleting a hash table, removing elements from the table, etc).

      3. The type of the hash table itself.  (More later.)

   In very special circumstances, users may need to know about a fourth type.

      4. The template type used to describe how hash table memory
      is allocated.  This type is called the allocator type.  It is
      parameterized on the value type.  It provides four functions.

         - A static member function named 'control_alloc'.  This function
         allocates the control data blocks for the table.

         - A static member function named 'control_free'.  This function
         frees the control data blocks for the table.

         - A static member function named 'data_alloc'.  This function
         allocates the data elements in the table.

         - A static member function named 'data_free'.  This function
         deallocates the data elements in the table.

   Hash table are instantiated with two type arguments.

      * The descriptor type, (2) above.

      * The allocator type, (4) above.  In general, you will not need to
      provide your own allocator type.  By default, hash tables will use
      the class template xcallocator, which uses malloc/free for allocation.


   DEFINING A DESCRIPTOR TYPE

   The first task in using the hash table is to describe the element type.
   We compose this into a few steps.

      1. Decide on a removal policy for values stored in the table.
         This header provides class templates for the two most common
         policies.

         * typed_free_remove implements the static 'remove' member function
         by calling free().

         * typed_noop_remove implements the static 'remove' member function
         by doing nothing.

         You can use these policies by simply deriving the descriptor type
         from one of those class template, with the appropriate argument.

         Otherwise, you need to write the static 'remove' member function
         in the descriptor class.

      2. Choose a hash function.  Write the static 'hash' member function.

      3. Choose an equality testing function.  In most cases, its two
      arguments will be value_type pointers.  If not, the first argument must
      be a value_type pointer, and the second argument a compare_type pointer.


   AN EXAMPLE DESCRIPTOR TYPE

   Suppose you want to put some_type into the hash table.  You could define
   the descriptor type as follows.

      struct some_type_hasher : typed_noop_remove <some_type>
      // Deriving from typed_noop_remove means that we get a 'remove' that does
      // nothing.  This choice is good for raw values.
      {
        typedef some_type value_type;
        typedef some_type compare_type;
        static inline hashval_t hash (const value_type *);
        static inline bool equal (const value_type *, const compare_type *);
      };

      inline hashval_t
      some_type_hasher::hash (const value_type *e)
      { ... compute and return a hash value for E ... }

      inline bool
      some_type_hasher::equal (const value_type *p1, const compare_type *p2)
      { ... compare P1 vs P2.  Return true if they are the 'same' ... }


   AN EXAMPLE HASH_TABLE DECLARATION

   To instantiate a hash table for some_type:

      hash_table <some_type_hasher> some_type_hash_table;

   There is no need to mention some_type directly, as the hash table will
   obtain it using some_type_hasher::value_type.

   You can then used any of the functions in hash_table's public interface.
   See hash_table for details.  The interface is very similar to libiberty's
   htab_t.


   EASY DESCRIPTORS FOR POINTERS

   The class template pointer_hash provides everything you need to hash
   pointers (as opposed to what they point to).  So, to instantiate a hash
   table over pointers to whatever_type,

      hash_table <pointer_hash <whatever_type>> whatever_type_hash_table;


   HASH TABLE ITERATORS

   The hash table provides standard C++ iterators.  For example, consider a
   hash table of some_info.  We wish to consume each element of the table:

      extern void consume (some_info *);

   We define a convenience typedef and the hash table:

      typedef hash_table <some_info_hasher> info_table_type;
      info_table_type info_table;

   Then we write the loop in typical C++ style:

      for (info_table_type::iterator iter = info_table.begin ();
           iter != info_table.end ();
           ++iter)
        if ((*iter).status == INFO_READY)
          consume (&*iter);

   Or with common sub-expression elimination:

      for (info_table_type::iterator iter = info_table.begin ();
           iter != info_table.end ();
           ++iter)
        {
          some_info &elem = *iter;
          if (elem.status == INFO_READY)
            consume (&elem);
        }

   One can also use a more typical GCC style:

      typedef some_info *some_info_p;
      some_info *elem_ptr;
      info_table_type::iterator iter;
      FOR_EACH_HASH_TABLE_ELEMENT (info_table, elem_ptr, some_info_p, iter)
        if (elem_ptr->status == INFO_READY)
          consume (elem_ptr);

*/


#ifndef TYPED_HASHTAB_H
#define TYPED_HASHTAB_H

#include "hashtab.h"


/* The ordinary memory allocator.  */
/* FIXME (crowl): This allocator may be extracted for wider sharing later.  */

template <typename Type>
struct xcallocator
{
  static Type *control_alloc (size_t count);
  static Type *data_alloc (size_t count);
  static void control_free (Type *memory);
  static void data_free (Type *memory);
};


/* Allocate memory for COUNT control blocks.  */

template <typename Type>
inline Type *
xcallocator <Type>::control_alloc (size_t count)
{
  return static_cast <Type *> (xcalloc (count, sizeof (Type)));
}


/* Allocate memory for COUNT data blocks.  */

template <typename Type>
inline Type *
xcallocator <Type>::data_alloc (size_t count)
{
  return static_cast <Type *> (xcalloc (count, sizeof (Type)));
}


/* Free memory for control blocks.  */

template <typename Type>
inline void
xcallocator <Type>::control_free (Type *memory)
{
  return ::free (memory);
}


/* Free memory for data blocks.  */

template <typename Type>
inline void
xcallocator <Type>::data_free (Type *memory)
{
  return ::free (memory);
}


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


/* Pointer hash with a no-op remove method.  */

template <typename Type>
struct pointer_hash : typed_noop_remove <Type>
{
  typedef Type value_type;
  typedef Type compare_type;

  static inline hashval_t
  hash (const value_type *);

  static inline int
  equal (const value_type *existing, const compare_type *candidate);
};

template <typename Type>
inline hashval_t
pointer_hash <Type>::hash (const value_type *candidate)
{
  /* This is a really poor hash function, but it is what the current code uses,
     so I am reusing it to avoid an additional axis in testing.  */
  return (hashval_t) ((intptr_t)candidate >> 3);
}

template <typename Type>
inline int
pointer_hash <Type>::equal (const value_type *existing,
			   const compare_type *candidate)
{
  return existing == candidate;
}


/* Table of primes and their inversion information.  */

struct prime_ent
{
  hashval_t prime;
  hashval_t inv;
  hashval_t inv_m2;     /* inverse of prime-2 */
  hashval_t shift;
};

extern struct prime_ent const prime_tab[];


/* Functions for computing hash table indexes.  */

extern unsigned int hash_table_higher_prime_index (unsigned long n);
extern hashval_t hash_table_mod1 (hashval_t hash, unsigned int index);
extern hashval_t hash_table_mod2 (hashval_t hash, unsigned int index);


/* Internal implementation type.  */

template <typename T>
struct hash_table_control
{
  /* Table itself.  */
  T **entries;

  /* Current size (in entries) of the hash table.  */
  size_t size;

  /* Current number of elements including also deleted elements.  */
  size_t n_elements;

  /* Current number of deleted elements in the table.  */
  size_t n_deleted;

  /* The following member is used for debugging. Its value is number
     of all calls of `htab_find_slot' for the hash table. */
  unsigned int searches;

  /* The following member is used for debugging.  Its value is number
     of collisions fixed for time of work with the hash table. */
  unsigned int collisions;

  /* Current size (in entries) of the hash table, as an index into the
     table of primes.  */
  unsigned int size_prime_index;
};


/* User-facing hash table type.

   The table stores elements of type Descriptor::value_type.

   It hashes values with the hash member function.
     The table currently works with relatively weak hash functions.
     Use typed_pointer_hash <Value> when hashing pointers instead of objects.

   It compares elements with the equal member function.
     Two elements with the same hash may not be equal.
     Use typed_pointer_equal <Value> when hashing pointers instead of objects.

   It removes elements with the remove member function.
     This feature is useful for freeing memory.
     Derive from typed_null_remove <Value> when not freeing objects.
     Derive from typed_free_remove <Value> when doing a simple object free.

   Specify the template Allocator to allocate and free memory.
     The default is xcallocator.

*/

template <typename Descriptor,
	  template <typename Type> class Allocator = xcallocator>
class hash_table
{
public:
  typedef typename Descriptor::value_type value_type;
  typedef typename Descriptor::compare_type compare_type;

  class iterator
  {
  public:
    inline iterator ();
    inline iterator (value_type **, value_type **);
    inline value_type &operator * ();
    void slide ();
    inline iterator &operator ++ ();
    inline bool operator != (const iterator &) const;
  private:
    value_type **m_slot;
    value_type **m_limit;
  };

private:
  hash_table_control <value_type> *htab;

  value_type **find_empty_slot_for_expand (hashval_t hash);
  void expand ();

public:
  hash_table ();
  void create (size_t initial_slots);
  bool is_created ();
  void dispose ();
  value_type *find (const value_type *value);
  value_type *find_with_hash (const compare_type *comparable, hashval_t hash);
  value_type **find_slot (const value_type *value, enum insert_option insert);
  value_type **find_slot_with_hash (const compare_type *comparable,
				    hashval_t hash, enum insert_option insert);
  void empty ();
  void clear_slot (value_type **slot);
  void remove_elt (const value_type *value);
  void remove_elt_with_hash (const compare_type *comparable, hashval_t hash);
  size_t size ();
  size_t elements ();
  size_t elements_with_deleted ();
  double collisions ();

  template <typename Argument,
	    int (*Callback) (value_type **slot, Argument argument)>
  void traverse_noresize (Argument argument);

  template <typename Argument,
	    int (*Callback) (value_type **slot, Argument argument)>
  void traverse (Argument argument);

  iterator begin ();
  iterator end ();
};


/* Construct the hash table.  The only useful operation next is create.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline
hash_table <Descriptor, Allocator>::hash_table ()
: htab (NULL)
{
}


/* See if the table has been created, as opposed to constructed.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline bool
hash_table <Descriptor, Allocator>::is_created ()
{
  return htab != NULL;
}


/* Like find_with_hash, but compute the hash value from the element.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline typename Descriptor::value_type *
hash_table <Descriptor, Allocator>::find (const value_type *value)
{
  return find_with_hash (value, Descriptor::hash (value));
}


/* Like find_slot_with_hash, but compute the hash value from the element.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline typename Descriptor::value_type **
hash_table <Descriptor, Allocator>
::find_slot (const value_type *value, enum insert_option insert)
{
  return find_slot_with_hash (value, Descriptor::hash (value), insert);
}


/* Like remove_elt_with_hash, but compute the hash value from the element.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline void
hash_table <Descriptor, Allocator>::remove_elt (const value_type *value)
{
  remove_elt_with_hash (value, Descriptor::hash (value));
}


/* Return the current size of this hash table.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline size_t
hash_table <Descriptor, Allocator>::size ()
{
  return htab->size;
}


/* Return the current number of elements in this hash table. */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline size_t
hash_table <Descriptor, Allocator>::elements ()
{
  return htab->n_elements - htab->n_deleted;
}


/* Return the current number of elements in this hash table. */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline size_t
hash_table <Descriptor, Allocator>::elements_with_deleted ()
{
  return htab->n_elements;
}


  /* Return the fraction of fixed collisions during all work with given
     hash table. */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline double
hash_table <Descriptor, Allocator>::collisions ()
{
  if (htab->searches == 0)
    return 0.0;

  return static_cast <double> (htab->collisions) / htab->searches;
}


/* Create a hash table with at least the given number of INITIAL_SLOTS.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
void
hash_table <Descriptor, Allocator>::create (size_t size)
{
  unsigned int size_prime_index;

  size_prime_index = hash_table_higher_prime_index (size);
  size = prime_tab[size_prime_index].prime;

  htab = Allocator <hash_table_control <value_type> > ::control_alloc (1);
  gcc_assert (htab != NULL);
  htab->entries = Allocator <value_type*> ::data_alloc (size);
  gcc_assert (htab->entries != NULL);
  htab->size = size;
  htab->size_prime_index = size_prime_index;
}


/* Dispose of a hash table.  Free all memory and return this hash table to
   the non-created state.  Naturally the hash table must already exist.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
void
hash_table <Descriptor, Allocator>::dispose ()
{
  size_t size = htab->size;
  value_type **entries = htab->entries;

  for (int i = size - 1; i >= 0; i--)
    if (entries[i] != HTAB_EMPTY_ENTRY && entries[i] != HTAB_DELETED_ENTRY)
      Descriptor::remove (entries[i]);

  Allocator <value_type *> ::data_free (entries);
  Allocator <hash_table_control <value_type> > ::control_free (htab);
  htab = NULL;
}


/* Similar to find_slot, but without several unwanted side effects:
    - Does not call equal when it finds an existing entry.
    - Does not change the count of elements/searches/collisions in the
      hash table.
   This function also assumes there are no deleted entries in the table.
   HASH is the hash value for the element to be inserted.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
typename Descriptor::value_type **
hash_table <Descriptor, Allocator>::find_empty_slot_for_expand (hashval_t hash)
{
  hashval_t index = hash_table_mod1 (hash, htab->size_prime_index);
  size_t size = htab->size;
  value_type **slot = htab->entries + index;
  hashval_t hash2;

  if (*slot == HTAB_EMPTY_ENTRY)
    return slot;
  else if (*slot == HTAB_DELETED_ENTRY)
    abort ();

  hash2 = hash_table_mod2 (hash, htab->size_prime_index);
  for (;;)
    {
      index += hash2;
      if (index >= size)
        index -= size;

      slot = htab->entries + index;
      if (*slot == HTAB_EMPTY_ENTRY)
        return slot;
      else if (*slot == HTAB_DELETED_ENTRY)
        abort ();
    }
}


/* The following function changes size of memory allocated for the
   entries and repeatedly inserts the table elements.  The occupancy
   of the table after the call will be about 50%.  Naturally the hash
   table must already exist.  Remember also that the place of the
   table entries is changed.  If memory allocation fails, this function
   will abort.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
void
hash_table <Descriptor, Allocator>::expand ()
{
  value_type **oentries;
  value_type **olimit;
  value_type **p;
  value_type **nentries;
  size_t nsize, osize, elts;
  unsigned int oindex, nindex;

  oentries = htab->entries;
  oindex = htab->size_prime_index;
  osize = htab->size;
  olimit = oentries + osize;
  elts = elements ();

  /* Resize only when table after removal of unused elements is either
     too full or too empty.  */
  if (elts * 2 > osize || (elts * 8 < osize && osize > 32))
    {
      nindex = hash_table_higher_prime_index (elts * 2);
      nsize = prime_tab[nindex].prime;
    }
  else
    {
      nindex = oindex;
      nsize = osize;
    }

  nentries = Allocator <value_type *> ::data_alloc (nsize);
  gcc_assert (nentries != NULL);
  htab->entries = nentries;
  htab->size = nsize;
  htab->size_prime_index = nindex;
  htab->n_elements -= htab->n_deleted;
  htab->n_deleted = 0;

  p = oentries;
  do
    {
      value_type *x = *p;

      if (x != HTAB_EMPTY_ENTRY && x != HTAB_DELETED_ENTRY)
        {
          value_type **q = find_empty_slot_for_expand (Descriptor::hash (x));

          *q = x;
        }

      p++;
    }
  while (p < olimit);

  Allocator <value_type *> ::data_free (oentries);
}


/* This function searches for a hash table entry equal to the given
   COMPARABLE element starting with the given HASH value.  It cannot
   be used to insert or delete an element. */

template <typename Descriptor,
	  template <typename Type> class Allocator>
typename Descriptor::value_type *
hash_table <Descriptor, Allocator>
::find_with_hash (const compare_type *comparable, hashval_t hash)
{
  hashval_t index, hash2;
  size_t size;
  value_type *entry;

  htab->searches++;
  size = htab->size;
  index = hash_table_mod1 (hash, htab->size_prime_index);

  entry = htab->entries[index];
  if (entry == HTAB_EMPTY_ENTRY
      || (entry != HTAB_DELETED_ENTRY && Descriptor::equal (entry, comparable)))
    return entry;

  hash2 = hash_table_mod2 (hash, htab->size_prime_index);
  for (;;)
    {
      htab->collisions++;
      index += hash2;
      if (index >= size)
        index -= size;

      entry = htab->entries[index];
      if (entry == HTAB_EMPTY_ENTRY
          || (entry != HTAB_DELETED_ENTRY
	      && Descriptor::equal (entry, comparable)))
        return entry;
    }
}


/* This function searches for a hash table slot containing an entry
   equal to the given COMPARABLE element and starting with the given
   HASH.  To delete an entry, call this with insert=NO_INSERT, then
   call clear_slot on the slot returned (possibly after doing some
   checks).  To insert an entry, call this with insert=INSERT, then
   write the value you want into the returned slot.  When inserting an
   entry, NULL may be returned if memory allocation fails. */

template <typename Descriptor,
	  template <typename Type> class Allocator>
typename Descriptor::value_type **
hash_table <Descriptor, Allocator>
::find_slot_with_hash (const compare_type *comparable, hashval_t hash,
		       enum insert_option insert)
{
  value_type **first_deleted_slot;
  hashval_t index, hash2;
  size_t size;
  value_type *entry;

  size = htab->size;
  if (insert == INSERT && size * 3 <= htab->n_elements * 4)
    {
      expand ();
      size = htab->size;
    }

  index = hash_table_mod1 (hash, htab->size_prime_index);

  htab->searches++;
  first_deleted_slot = NULL;

  entry = htab->entries[index];
  if (entry == HTAB_EMPTY_ENTRY)
    goto empty_entry;
  else if (entry == HTAB_DELETED_ENTRY)
    first_deleted_slot = &htab->entries[index];
  else if (Descriptor::equal (entry, comparable))
    return &htab->entries[index];

  hash2 = hash_table_mod2 (hash, htab->size_prime_index);
  for (;;)
    {
      htab->collisions++;
      index += hash2;
      if (index >= size)
	index -= size;

      entry = htab->entries[index];
      if (entry == HTAB_EMPTY_ENTRY)
	goto empty_entry;
      else if (entry == HTAB_DELETED_ENTRY)
	{
	  if (!first_deleted_slot)
	    first_deleted_slot = &htab->entries[index];
	}
      else if (Descriptor::equal (entry, comparable))
	return &htab->entries[index];
    }

 empty_entry:
  if (insert == NO_INSERT)
    return NULL;

  if (first_deleted_slot)
    {
      htab->n_deleted--;
      *first_deleted_slot = static_cast <value_type *> (HTAB_EMPTY_ENTRY);
      return first_deleted_slot;
    }

  htab->n_elements++;
  return &htab->entries[index];
}


/* This function clears all entries in the given hash table.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
void
hash_table <Descriptor, Allocator>::empty ()
{
  size_t size = htab->size;
  value_type **entries = htab->entries;
  int i;

  for (i = size - 1; i >= 0; i--)
    if (entries[i] != HTAB_EMPTY_ENTRY && entries[i] != HTAB_DELETED_ENTRY)
      Descriptor::remove (entries[i]);

  /* Instead of clearing megabyte, downsize the table.  */
  if (size > 1024*1024 / sizeof (PTR))
    {
      int nindex = hash_table_higher_prime_index (1024 / sizeof (PTR));
      int nsize = prime_tab[nindex].prime;

      Allocator <value_type *> ::data_free (htab->entries);
      htab->entries = Allocator <value_type *> ::data_alloc (nsize);
      htab->size = nsize;
      htab->size_prime_index = nindex;
    }
  else
    memset (entries, 0, size * sizeof (value_type *));
  htab->n_deleted = 0;
  htab->n_elements = 0;
}


/* This function clears a specified SLOT in a hash table.  It is
   useful when you've already done the lookup and don't want to do it
   again. */

template <typename Descriptor,
	  template <typename Type> class Allocator>
void
hash_table <Descriptor, Allocator>::clear_slot (value_type **slot)
{
  if (slot < htab->entries || slot >= htab->entries + htab->size
      || *slot == HTAB_EMPTY_ENTRY || *slot == HTAB_DELETED_ENTRY)
    abort ();

  Descriptor::remove (*slot);

  *slot = static_cast <value_type *> (HTAB_DELETED_ENTRY);
  htab->n_deleted++;
}


/* This function deletes an element with the given COMPARABLE value
   from hash table starting with the given HASH.  If there is no
   matching element in the hash table, this function does nothing. */

template <typename Descriptor,
	  template <typename Type> class Allocator>
void
hash_table <Descriptor, Allocator>
::remove_elt_with_hash (const compare_type *comparable, hashval_t hash)
{
  value_type **slot;

  slot = find_slot_with_hash (comparable, hash, NO_INSERT);
  if (*slot == HTAB_EMPTY_ENTRY)
    return;

  Descriptor::remove (*slot);

  *slot = static_cast <value_type *> (HTAB_DELETED_ENTRY);
  htab->n_deleted++;
}


/* This function scans over the entire hash table calling CALLBACK for
   each live entry.  If CALLBACK returns false, the iteration stops.
   ARGUMENT is passed as CALLBACK's second argument. */

template <typename Descriptor,
	  template <typename Type> class Allocator>
template <typename Argument,
	  int (*Callback) (typename Descriptor::value_type **slot, Argument argument)>
void
hash_table <Descriptor, Allocator>::traverse_noresize (Argument argument)
{
  value_type **slot;
  value_type **limit;

  slot = htab->entries;
  limit = slot + htab->size;

  do
    {
      value_type *x = *slot;

      if (x != HTAB_EMPTY_ENTRY && x != HTAB_DELETED_ENTRY)
        if (! Callback (slot, argument))
          break;
    }
  while (++slot < limit);
}


/* Like traverse_noresize, but does resize the table when it is too empty
   to improve effectivity of subsequent calls.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
template <typename Argument,
	  int (*Callback) (typename Descriptor::value_type **slot,
			   Argument argument)>
void
hash_table <Descriptor, Allocator>::traverse (Argument argument)
{
  size_t size = htab->size;
  if (elements () * 8 < size && size > 32)
    expand ();

  traverse_noresize <Argument, Callback> (argument);
}


/* Iterator definitions.  */

/* The default constructor produces the end value.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline
hash_table <Descriptor, Allocator>::iterator::iterator ()
: m_slot (NULL), m_limit (NULL)
{
}

/* The parameterized constructor produces the begin value.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline
hash_table <Descriptor, Allocator>::iterator::iterator
   (value_type **slot, value_type **limit)
: m_slot (slot), m_limit (limit)
{
}

/* Obtain the element.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline typename hash_table <Descriptor, Allocator>::value_type &
hash_table <Descriptor, Allocator>::iterator::operator * ()
{
  return **m_slot;
}

/* Slide down the iterator slots until an active entry is found.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
void
hash_table <Descriptor, Allocator>::iterator::slide ()
{
  for ( ; m_slot < m_limit; ++m_slot )
    {
      value_type *x = *m_slot;
      if (x != HTAB_EMPTY_ENTRY && x != HTAB_DELETED_ENTRY)
        return;
    }
  m_slot = NULL;
  m_limit = NULL;
}

/* Bump the iterator.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline typename hash_table <Descriptor, Allocator>::iterator &
hash_table <Descriptor, Allocator>::iterator::operator ++ ()
{
  ++m_slot;
  slide ();
  return *this;
}

/* Compare iterators.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline bool
hash_table <Descriptor, Allocator>::iterator::
  operator != (const iterator &other) const
{
  return m_slot != other.m_slot || m_limit != other.m_limit;
}

/* Hash table iterator producers.  */

/* The beginning of a hash table iteration.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline typename hash_table <Descriptor, Allocator>::iterator
hash_table <Descriptor, Allocator>::begin ()
{
  iterator hti (htab->entries, htab->entries + htab->size);
  hti.slide ();
  return hti;
}

/* The end of a hash table iteration.  */

template <typename Descriptor,
	  template <typename Type> class Allocator>
inline typename hash_table <Descriptor, Allocator>::iterator
hash_table <Descriptor, Allocator>::end ()
{
  return iterator ();
}

/* Iterate through the elements of hash_table HTAB,
   using hash_table <....>::iterator ITER,
   storing each element in RESULT, which is of type TYPE.

   This macro has this form for compatibility with the
   FOR_EACH_HTAB_ELEMENT currently defined in tree-flow.h.  */

#define FOR_EACH_HASH_TABLE_ELEMENT(HTAB, RESULT, TYPE, ITER) \
  for ((ITER) = (HTAB).begin (); \
       (ITER) != (HTAB).end () ? (RESULT = &*(ITER) , true) : false; \
       ++(ITER))

#endif /* TYPED_HASHTAB_H */
