/* Copyright (C) 1989, 1992 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

/* 
  $Header: /home/fsf/rms/c-runtime/dispatch/RCS/hash.h,v 0.12 1992/09/02 01:59:40 rms Exp rms $
  $Author: rms $
  $Date: 1992/09/02 01:59:40 $
  $Log: hash.h,v $
 * Revision 0.12  1992/09/02  01:59:40  rms
 * Changed the format of various sections to conform with GNU standard.
 * Deleted dependencies on some header files.
 * Replaced the use of the functions from memory.h with funtions like bzero.
 * Changed the include format.
 *
 * Revision 0.11  1992/08/31  21:15:02  dglattin
 * minor documentation changes.
 *
 * Revision 0.10  1992/08/18  04:46:58  dglattin
 * Saving a working version before release.
 *
 * Revision 0.9  1992/04/13  11:43:08  dennisg
 * Check in after array version of run-time works.
 * Expect more changes as hash version and other changes are made.
 *
 * Revision 0.8  1991/12/10  12:05:28  dennisg
 * Cleaned up file format for a distribution.
 *
 * Revision 0.7  1991/12/03  02:01:23  dennisg
 * fixed assert macro.
 * added memory allocation adjustment macro for hash size allocation.
 *
 * Revision 0.6  1991/11/24  01:20:02  dennisg
 * changed shorts back to ints.
 * the efficiency gained didn't out weight the grossness of the code.
 *
 * Revision 0.5  1991/11/23  22:19:21  dennisg
 * converted some entries in the hash structure from ints to shorts.
 * this was done to use a less expensive division instruction
 * in the hashIndex  routine.
 *
 * Revision 0.4  1991/11/21  22:25:19  dennisg
 * deleted hash mask information from hash struct.
 * changed hashing algorithm.  those values are no longer needed.
 *
 * Revision 0.3  1991/11/07  23:23:40  dennisg
 * implemented hash table expansion as suggested by rms.
 *
 * Revision 0.2  1991/11/07  22:30:54  dennisg
 * added copyleft
 *
 * Revision 0.1  1991/10/24  00:45:39  dennisg
 * Initial check in.  Preliminary development stage.
 *
*/
 

#ifndef _hash_INCLUDE_GNU
#define _hash_INCLUDE_GNU


#include "assert.h"
#include "mutex.h"


/*
 * This data structure is used to hold items
 *  stored in a hash table.  Each node holds 
 *  a key/value pair.
 *
 * Items in the cache are really of type void*.
 */
typedef struct cache_node {
  struct cache_node *next;	/* Pointer to next entry on the list.
				   NULL indicates end of list. */
  void *key;			/* Key used to locate the value.  Used
				   to locate value when more than one
				   key computes the same hash
				   value. */
  void *value;			/* Value stored for the key. */
} *node_ptr;


/*
 * This data type is the function that computes a hash code given a key.
 * Therefore, the key can be a pointer to anything and the function specific
 * to the key type. 
 *
 * Unfortunately there is a mutual data structure reference problem with this
 * typedef.  Therefore, to remove compiler warnings the functions passed to
 * hash_new will have to be casted to this type. 
 */
typedef unsigned int   (*hash_func_type)(void*, void*);

/*
 * This data type is the function that compares two hash keys and returns an
 * integer greater than, equal to, or less than 0, according as the first
 * parameter is lexico-graphically greater than, equal to, or less than the
 * second. 
 */

typedef int     (*compare_func_type)(void*, void*);


/*
 * This data structure is the cache.
 *
 * It must be passed to all of the hashing routines
 *   (except for new).
 */
typedef struct cache {
  /* Variables used to implement the hash itself.  */
  node_ptr  (*node_table)[]; /* Pointer to an array of hash nodes.  */
  /* Variables used to track the size of the hash table so to determine
    when to resize it.  */
  unsigned int size; /* Number of buckets allocated for the hash table
			(number of array entries allocated for
			"node_table").  Must be a power of two.  */
  unsigned int used; /* Current number of entries in the hash table.  */
  unsigned int mask; /* Precomputed mask.  */

  /* Variables used to implement indexing through the hash table.  */

  unsigned int last_bucket; /* Tracks which entry in the array where
			       the last value was returned.  */
  /* Function used to compute a hash code given a key. 
     This function is specified when the hash table is created.  */
  hash_func_type    hash_func;
  /* Function used to compare two hash keys to see if they are equal.  */
  compare_func_type compare_func;
} *cache_ptr;


/* Allocate and initialize a hash table.  */ 

cache_ptr hash_new (unsigned int size,
		    hash_func_type hash_func, compare_func_type compare_func);
                       
/* Deallocate all of the hash nodes and the cache itself.  */

void hash_delete (cache_ptr cache);

/* Add the key/value pair to the hash table.  If the
   hash table reaches a level of fullnes then it will be resized. 
                                                   
   assert if the key is already in the hash.  */

void hash_add (cache_ptr *cachep, void *key, void *value);
     
/* Remove the key/value pair from the hash table.  
   assert if the key isn't in the table.  */

void hash_remove (cache_ptr cache, void *key);

/* Used to index through the hash table.  Start with NULL
   to get the first entry.
                                                  
   Successive calls pass the value returned previously.
   ** Don't modify the hash during this operation *** 
                                                  
   Cache nodes are returned such that key or value can
   be extracted.  */

node_ptr hash_next (cache_ptr cache, node_ptr node);

/* Used to return a value from a hash table using a given key.  */

void *hash_value_for_key (cache_ptr cache, void *key);


/************************************************

        Useful hashing functions.  
        
        Declared inline for your pleaseure. 
        
************************************************/

/* Calculate a hash code by performing some 
   manipulation of the key pointer.  */
static inline unsigned int 
hash_int (cache_ptr cache, void *key)
{
  assert (sizeof (unsigned int) == sizeof (key));

  return ((unsigned int)key >> (sizeof (void *) - 1)) & cache->mask;
}


/* Calculate a hash code by iterating over a NULL 
   terminate string.  */
static inline unsigned int 
hash_string (cache_ptr cache, void *key)
{
  unsigned int ret = 0;
  unsigned int ctr = 0;
        
        
  while (*(char*)key) {
    ret ^= *(char*)key++ << ctr;
    ctr = (ctr + 1) % sizeof (void *);
  }

  return ret & cache->mask;
}


/* Compare two integers.  */
static inline int 
compare_ints (void *k1, void *k2)
{
  return !((int)k1 - (int)k2);
}


/* Compare two strings.  */
static inline int 
compare_strings (void *k1, void *k2)
{
  return !strcmp (k1, k2);
}


#endif /* _hash_INCLUDE_GNU */
