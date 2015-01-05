/* An incremental hash abstract data type.
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

#ifndef INCHASH_H
#define INCHASH_H 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "hashtab.h"

/* This file implements an incremential hash function ADT, to be used
   by code that incrementially hashes a lot of unrelated data
   (not in a single memory block) into a single value. The goal
   is to make it easy to plug in efficient hash algorithms.
   Currently it just implements the plain old jhash based
   incremental hash from gcc's tree.c.  */

extern hashval_t iterative_hash_host_wide_int (HOST_WIDE_INT, hashval_t);
extern hashval_t iterative_hash_hashval_t (hashval_t, hashval_t);

namespace inchash
{

class hash
{
 public:

  /* Start incremential hashing, optionally with SEED.  */
  hash (hashval_t seed = 0)
  {
    val = seed;
    bits = 0;
  }

  /* End incremential hashing and provide the final value.  */
  hashval_t end ()
  {
    return val;
  }

  /* Add unsigned value V.  */
  void add_int (unsigned v)
  {
    val = iterative_hash_hashval_t (v, val);
  }

  /* Add HOST_WIDE_INT value V.  */
  void add_wide_int (HOST_WIDE_INT v)
  {
    val = iterative_hash_host_wide_int (v, val);
  }

  /* Hash in pointer PTR.  */
  void add_ptr (void *ptr)
  {
    add (&ptr, sizeof (ptr));
  }

  /* Add a memory block DATA with size LEN.  */
  void add (const void *data, size_t len)
  {
    val = iterative_hash (data, len, val);
  }

  /* Merge hash value OTHER.  */
  void merge_hash (hashval_t other)
  {
    val = iterative_hash_hashval_t (other, val);
  }

  /* Hash in state from other inchash OTHER.  */
  void merge (hash &other)
  {
    merge_hash (other.val);
  }

  template<class T> void add_object(T &obj)
  {
    add (&obj, sizeof(T));
  }

  /* Support for accumulating boolean flags */

  void add_flag (bool flag)
  {
    bits = (bits << 1) | flag;
  }

  void commit_flag ()
  {
    add_int (bits);
    bits = 0;
  }

  /* Support for commutative hashing. Add A and B in a defined order
     based on their value. This is useful for hashing commutative
     expressions, so that A+B and B+A get the same hash.  */

  void add_commutative (hash &a, hash &b)
  {
    if (a.end() > b.end())
      {
        merge (b);
	merge (a);
      }
    else
      {
        merge (a);
        merge (b);
      }
  }

 private:
  hashval_t val;
  unsigned bits;
};

}

#endif
