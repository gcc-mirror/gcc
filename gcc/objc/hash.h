/* -*-c-*-
 * This is a general purpose hash object.
 *
 * The hash object used throughout the run-time
 *  is an integer hash.  The key and data is of type
 *  void*.  The hashing function converts the key to
 *  an integer and computes it hash value.
 *
 * Copyright (C) 1991 Threaded Technologies Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 1, or any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should receive a copy of the GNU General Public License 
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 * 
  $Header: /usr/user/dennis_glatting/ObjC/c-runtime/lib/RCS/hash.h,v 0.2 1991/11/07 22:30:54 dennisg Exp dennisg $
  $Author: dennisg $
  $Date: 1991/11/07 22:30:54 $
  $Log: hash.h,v $
 * Revision 0.2  1991/11/07  22:30:54  dennisg
 * added copyleft
 *
 * Revision 0.1  1991/10/24  00:45:39  dennisg
 * Initial check in.  Preliminary development stage.
 *
*/
 

#ifndef _hash_INCLUDE_GNU
#define _hash_INCLUDE_GNU

                                                /* If someone is using a c++
                                                  compiler then adjust the 
                                                  types in the file back 
                                                  to C. */
#ifdef __cplusplus
extern "C" {
#endif

#include  <sys/types.h>


/*
 * This data structure is used to hold items
 *  stored in a hash table.  Each node holds 
 *  a key/value pair.
 *
 * Items in the cache are really of type void*.
 */
typedef struct cache_node {
  struct cache_node*  nextNode;                   /* Pointer to next entry on
                                                    the list.  NULL indicates
                                                    end of list. */
  void*               theKey;                     /* Key used to locate the
                                                    value.  Used to locate
                                                    value when more than one
                                                    key computes the same hash
                                                    value. */
  void*               theValue;                   /* Value stored for the
                                                    key. */
} CacheNode, *CacheNode_t;


/*
 * This data structure is the cache.
 *
 * It must be passed to all of the hashing routines
 *  (except for new).
 */
typedef struct cache {
  /*
   * Variables used to implement the
   *  hash itself.
   */
  CacheNode_t (* theNodeTable )[];                /* Pointer to an array of
                                                    hash nodes. */
	/*
	 * Variables used to track the size of the hash
	 *	table so to determine when to resize it.
	 */
  u_int       sizeOfHash,                         /* Number of buckets 
                                                    allocated for the hash
                                                    table (number of array
                                                    entries allocated for
                                                    "theNodeTable"). */
							entriesInHash;											/* Current number of entries
																										in ther hash table. */
	/*
	 * Variables used to compute hash
	 *	values.
	 */
  u_int       mask,                               /* The number of bits set 
																										in the mask that is
																										contained in the next 
																										member variable. */
              numberOfMaskBits;                   /* Number of bits used for
                                                    the mask.  Useful for 
                                                    efficient hash value
                                                    calculation. */
  /*
   * Variables used to implement indexing
   *  through the hash table.
   */
  u_int       lastBucket;                         /* Tracks which entry in the
                                                    array where the last value
                                                    was returned. */
} Cache, *Cache_t;


                                                /* Prototypes for hash
                                                  functions. */
                                                /* Allocate and initialize 
                                                  a hash table.  Hash table 
                                                  size taken as a parameter. 
                                                    A value of 0 is not 
                                                  allowed. */ 
Cache_t hash_new( u_int sizeOfHash );
                                                /* Deallocate all of the
                                                  hash nodes and the cache
                                                  itself. */
void hash_delete( Cache_t theCache );
                                                /* Add the key/value pair
                                                  to the hash table.  If the
																									hash table reaches a 
																									level of fullnes then
																									it will be resized. 
																									 
																									assert() if the key is 
																									already in the hash. */
void hash_add( Cache_t* theCache, void* aKey, void* aValue );
                                                /* Remove the key/value pair
                                                  from the hash table.  
                                                  assert() if the key isn't 
                                                  in the table. */
void hash_remove( Cache_t theCache, void* aKey );
                                                /* Given key, return its 
                                                  value.  Return NULL if the
                                                  key/value pair isn't in
                                                  the hash. */
void* hash_value_for_key( Cache_t theCache, void* aKey );
                                                /* Used to index through the
                                                  hash table.  Start with NULL
                                                  to get the first entry.
                                                  
                                                  Successive calls pass the
                                                  value returned previously.
                                                  ** Don't modify the hash
                                                  during this operation *** 
                                                  
                                                  Cache nodes are returned
                                                  such that key or value can
                                                  ber extracted. */
CacheNode_t hash_next( Cache_t theCache, CacheNode_t aCacheNode );


#ifdef __cplusplus
}
#endif

#endif
