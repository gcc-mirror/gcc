/* A type-safe hash table template.
   Copyright (C) 2012
   Free Software Foundation, Inc.
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
   The implementation borrows from libiberty's hashtab.  */


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


/* A common function for hashing a CANDIDATE typed pointer.  */

template <typename Element>
inline hashval_t
typed_pointer_hash (const Element *candidate)
{
  /* This is a really poor hash function, but it is what the current code uses,
     so I am reusing it to avoid an additional axis in testing.  */
  return (hashval_t) ((intptr_t)candidate >> 3);
}


/* A common function for comparing an EXISTING and CANDIDATE typed pointers
   for equality. */

template <typename Element>
inline int
typed_pointer_equal (const Element *existing, const Element * candidate)
{
  return existing == candidate;
}


/* A common function for doing nothing on removing a RETIRED slot.  */

template <typename Element>
inline void
typed_null_remove (Element *retired ATTRIBUTE_UNUSED)
{
}


/* A common function for using free on removing a RETIRED slot.  */

template <typename Element>
inline void
typed_free_remove (Element *retired)
{
  free (retired);
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

template <typename Element>
struct hash_table_control
{
  /* Table itself.  */
  Element **entries;

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

   The table stores elements of type Element.

   It hashes elements with the Hash function.
     The table currently works with relatively weak hash functions.
     Use typed_pointer_hash <Element> when hashing pointers instead of objects.

   It compares elements with the Equal function.
     Two elements with the same hash may not be equal.
     Use typed_pointer_equal <Element> when hashing pointers instead of objects.

   It removes elements with the Remove function.
     This feature is useful for freeing memory.
     Use typed_null_remove <Element> when not freeing objects.
     Use typed_free_remove <Element> when doing a simple object free.

   Use the Allocator template to allocate and free memory.
     The default is xcallocator.

*/

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator = xcallocator>
class hash_table
{

private:

  hash_table_control <Element> *htab;

  Element **find_empty_slot_for_expand (hashval_t hash);
  void expand ();

public:

  hash_table ();
  void create (size_t initial_slots);
  bool is_created ();
  void dispose ();
  Element *find (Element *comparable);
  Element *find_with_hash (Element *comparable, hashval_t hash);
  Element **find_slot (Element *comparable, enum insert_option insert);
  Element **find_slot_with_hash (Element *comparable, hashval_t hash,
				 enum insert_option insert);
  void empty ();
  void clear_slot (Element **slot);
  void remove_elt (Element *comparable);
  void remove_elt_with_hash (Element *comparable, hashval_t hash);
  size_t size();
  size_t elements();
  double collisions();

  template <typename Argument,
	    int (*Callback) (Element **slot, Argument argument)>
  void traverse_noresize (Argument argument);

  template <typename Argument,
	    int (*Callback) (Element **slot, Argument argument)>
  void traverse (Argument argument);
};


/* Construct the hash table.  The only useful operation next is create.  */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
inline
hash_table <Element, Hash, Equal, Remove, Allocator>::hash_table ()
: htab (NULL)
{
}


/* See if the table has been created, as opposed to constructed.  */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
inline bool
hash_table <Element, Hash, Equal, Remove, Allocator>::is_created ()
{
  return htab != NULL;
}


/* Like find_with_hash, but compute the hash value from the element.  */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
inline Element *
hash_table <Element, Hash, Equal, Remove, Allocator>::find (Element *comparable)
{
  return find_with_hash (comparable, Hash (comparable));
}


/* Like find_slot_with_hash, but compute the hash value from the element.  */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
inline Element **
hash_table <Element, Hash, Equal, Remove, Allocator>
::find_slot (Element *comparable, enum insert_option insert)
{
  return find_slot_with_hash (comparable, Hash (comparable), insert);
}


/* Like remove_elt_with_hash, but compute the hash value from the element.  */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
inline void
hash_table <Element, Hash, Equal, Remove, Allocator>
::remove_elt (Element *comparable)
{
  remove_elt_with_hash (comparable, Hash (comparable));
}


/* Return the current size of this hash table.  */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
inline size_t
hash_table <Element, Hash, Equal, Remove, Allocator>::size()
{
  return htab->size;
}


/* Return the current number of elements in this hash table. */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
inline size_t
hash_table <Element, Hash, Equal, Remove, Allocator>::elements()
{
  return htab->n_elements - htab->n_deleted;
}


  /* Return the fraction of fixed collisions during all work with given
     hash table. */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
inline double
hash_table <Element, Hash, Equal, Remove, Allocator>::collisions()
{
  if (htab->searches == 0)
    return 0.0;

  return static_cast <double> (htab->collisions) / htab->searches;
}


/* Create a hash table with at least the given number of INITIAL_SLOTS.  */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
void
hash_table <Element, Hash, Equal, Remove, Allocator>::create (size_t size)
{
  unsigned int size_prime_index;

  size_prime_index = hash_table_higher_prime_index (size);
  size = prime_tab[size_prime_index].prime;

  htab = Allocator <hash_table_control <Element> > ::control_alloc (1);
  gcc_assert (htab != NULL);
  htab->entries = Allocator <Element*> ::data_alloc (size);
  gcc_assert (htab->entries != NULL);
  htab->size = size;
  htab->size_prime_index = size_prime_index;
}


/* Dispose of a hash table.  Free all memory and return this hash table to
   the non-created state.  Naturally the hash table must already exist.  */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
void
hash_table <Element, Hash, Equal, Remove, Allocator>::dispose ()
{
  size_t size = htab->size;
  Element **entries = htab->entries;

  for (int i = size - 1; i >= 0; i--)
    if (entries[i] != HTAB_EMPTY_ENTRY && entries[i] != HTAB_DELETED_ENTRY)
      Remove (entries[i]);

  Allocator <Element *> ::data_free (entries);
  Allocator <hash_table_control <Element> > ::control_free (htab);
  htab = NULL;
}


/* Similar to find_slot, but without several unwanted side effects:
    - Does not call Equal when it finds an existing entry.
    - Does not change the count of elements/searches/collisions in the
      hash table.
   This function also assumes there are no deleted entries in the table.
   HASH is the hash value for the element to be inserted.  */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
Element **
hash_table <Element, Hash, Equal, Remove, Allocator>
::find_empty_slot_for_expand (hashval_t hash)
{
  hashval_t index = hash_table_mod1 (hash, htab->size_prime_index);
  size_t size = htab->size;
  Element **slot = htab->entries + index;
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

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
void
hash_table <Element, Hash, Equal, Remove, Allocator>::expand ()
{
  Element **oentries;
  Element **olimit;
  Element **p;
  Element **nentries;
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

  nentries = Allocator <Element *> ::data_alloc (nsize);
  gcc_assert (nentries != NULL);
  htab->entries = nentries;
  htab->size = nsize;
  htab->size_prime_index = nindex;
  htab->n_elements -= htab->n_deleted;
  htab->n_deleted = 0;

  p = oentries;
  do
    {
      Element *x = *p;

      if (x != HTAB_EMPTY_ENTRY && x != HTAB_DELETED_ENTRY)
        {
          Element **q = find_empty_slot_for_expand (Hash (x));

          *q = x;
        }

      p++;
    }
  while (p < olimit);

  Allocator <Element *> ::data_free (oentries);
}


/* This function searches for a hash table entry equal to the given
   COMPARABLE element starting with the given HASH value.  It cannot
   be used to insert or delete an element. */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
Element *
hash_table <Element, Hash, Equal, Remove, Allocator>
::find_with_hash (Element *comparable, hashval_t hash)
{
  hashval_t index, hash2;
  size_t size;
  Element *entry;

  htab->searches++;
  size = htab->size;
  index = hash_table_mod1 (hash, htab->size_prime_index);

  entry = htab->entries[index];
  if (entry == HTAB_EMPTY_ENTRY
      || (entry != HTAB_DELETED_ENTRY && Equal (entry, comparable)))
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
          || (entry != HTAB_DELETED_ENTRY && Equal (entry, comparable)))
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

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
Element **
hash_table <Element, Hash, Equal, Remove, Allocator>
::find_slot_with_hash (Element *comparable, hashval_t hash,
		       enum insert_option insert)
{
  Element **first_deleted_slot;
  hashval_t index, hash2;
  size_t size;
  Element *entry;

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
  else if (Equal (entry, comparable))
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
      else if (Equal (entry, comparable))
	return &htab->entries[index];
    }

 empty_entry:
  if (insert == NO_INSERT)
    return NULL;

  if (first_deleted_slot)
    {
      htab->n_deleted--;
      *first_deleted_slot = static_cast <Element *> (HTAB_EMPTY_ENTRY);
      return first_deleted_slot;
    }

  htab->n_elements++;
  return &htab->entries[index];
}


/* This function clears all entries in the given hash table.  */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
void
hash_table <Element, Hash, Equal, Remove, Allocator>::empty ()
{
  size_t size = htab_size (htab);
  Element **entries = htab->entries;
  int i;

  for (i = size - 1; i >= 0; i--)
    if (entries[i] != HTAB_EMPTY_ENTRY && entries[i] != HTAB_DELETED_ENTRY)
      Remove (entries[i]);

  /* Instead of clearing megabyte, downsize the table.  */
  if (size > 1024*1024 / sizeof (PTR))
    {
      int nindex = hash_table_higher_prime_index (1024 / sizeof (PTR));
      int nsize = prime_tab[nindex].prime;

      Allocator <Element *> ::data_free (htab->entries);
      htab->entries = Allocator <Element *> ::data_alloc (nsize);
      htab->size = nsize;
      htab->size_prime_index = nindex;
    }
  else
    memset (entries, 0, size * sizeof (Element *));
  htab->n_deleted = 0;
  htab->n_elements = 0;
}


/* This function clears a specified SLOT in a hash table.  It is
   useful when you've already done the lookup and don't want to do it
   again. */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
void
hash_table <Element, Hash, Equal, Remove, Allocator>
::clear_slot (Element **slot)
{
  if (slot < htab->entries || slot >= htab->entries + htab->size
      || *slot == HTAB_EMPTY_ENTRY || *slot == HTAB_DELETED_ENTRY)
    abort ();

  Remove (*slot);

  *slot = HTAB_DELETED_ENTRY;
  htab->n_deleted++;
}


/* This function deletes an element with the given COMPARABLE value
   from hash table starting with the given HASH.  If there is no
   matching element in the hash table, this function does nothing. */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
void
hash_table <Element, Hash, Equal, Remove, Allocator>
::remove_elt_with_hash (Element *comparable, hashval_t hash)
{
  Element **slot;

  slot = find_slot_with_hash (comparable, hash, NO_INSERT);
  if (*slot == HTAB_EMPTY_ENTRY)
    return;

  Remove (*slot);

  *slot = static_cast <Element *> (HTAB_DELETED_ENTRY);
  htab->n_deleted++;
}


/* This function scans over the entire hash table calling CALLBACK for
   each live entry.  If CALLBACK returns false, the iteration stops.
   ARGUMENT is passed as CALLBACK's second argument. */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
template <typename Argument,
	  int (*Callback) (Element **slot, Argument argument)>
void
hash_table <Element, Hash, Equal, Remove, Allocator>
::traverse_noresize (Argument argument)
{
  Element **slot;
  Element **limit;

  slot = htab->entries;
  limit = slot + htab->size;

  do
    {
      Element *x = *slot;

      if (x != HTAB_EMPTY_ENTRY && x != HTAB_DELETED_ENTRY)
        if (! Callback (slot, argument))
          break;
    }
  while (++slot < limit);
}


/* Like traverse_noresize, but does resize the table when it is too empty
   to improve effectivity of subsequent calls.  */

template <typename Element,
	  hashval_t (*Hash) (const Element *candidate),
	  int (*Equal) (const Element *existing, const Element * candidate),
	  void (*Remove) (Element *retired),
	  template <typename Type> class Allocator>
template <typename Argument,
	  int (*Callback) (Element **slot, Argument argument)>
void
hash_table <Element, Hash, Equal, Remove, Allocator>
::traverse (Argument argument)
{
  size_t size = htab->size;
  if (elements () * 8 < size && size > 32)
    expand ();

  traverse_noresize <Argument, Callback> (argument);
}

#endif /* TYPED_HASHTAB_H */
