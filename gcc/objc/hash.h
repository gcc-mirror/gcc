/* -*-c-*- */

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
  $Header: /usr/user/dennis_glatting/ObjC/c-runtime/dispatch/RCS/hash.h,v 0.10 1992/08/18 04:46:58 dglattin Exp $
  $Author: dglattin $
  $Date: 1992/08/18 04:46:58 $
  $Log: hash.h,v $
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
 * in the hashIndex () routine.
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

                                                /* If someone is using a c++
                                                  compiler then adjust the 
                                                  types in the file back 
                                                  to C. */
#ifdef __cplusplus
extern "C" {
#endif

#include        <assert.h>
#include  <sys/types.h>

#include        <mutex.h>

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
 * This data type is the function that computes a hash code given a key.
 * Therefore, the key can be a pointer to anything and the function specific
 * to the key type. 
 *
 * Unfortunately there is a mutual data structure reference problem with this
 * typedef.  Therefore, to remove compiler warnings the functions passed to
 * hash_new() will have to be casted to this type. 
 */
typedef u_int   (*HashFunc)(void*, void*);

/*
 * This data type is the function that compares two hash keys and returns an
 * integer greater than, equal to, or less than 0, according as the first
 * parameter is lexico-graphically greater than, equal to, or less than the
 * second. 
 */

typedef int     (*CompareFunc)(void*, void*);


/*
 * This data structure is the cache.
 *
 * It must be passed to all of the hashing routines
 *   (except for new).
 */
typedef struct cache {
  /*
   * Variables used to implement the
   *  hash itself.
   */
  CacheNode_t  (* theNodeTable)[];                /* Pointer to an array of
                                                    hash nodes. */
  /*
   * Variables used to track the size of the hash
   *  table so to determine when to resize it.
   */
  u_int       sizeOfHash,                        /* Number of buckets 
                                                    allocated for the hash
                                                    table  (number of array
                                                    entries allocated for
                                                    "theNodeTable").  Must be
                                                    a power of two. */
              entriesInHash,                      /* Current number of entries
                                                    in the hash table. */
              mask;                               /* Precomputed mask. */
  /*
   * Variables used to implement indexing
   *  through the hash table.
   */
  u_int       lastBucket;                         /* Tracks which entry in the
                                                    array where the last value
                                                    was returned. */
                                                  /* Function used to compute
						     a hash code given a key. 
						     This function is 
						     specified when the hash 
						     table is created. */
  HashFunc    hashFunc;
                                                  /* Function used to compare 
						     two hash keys to determine
						     if they are equal. */
  CompareFunc compareFunc;
} Cache, *Cache_t;


                                                /* Prototypes for hash
                                                  functions. */
                                                /* Allocate and initialize 
                                                  a hash table. */ 
Cache_t 
hash_new (u_int sizeOfHash, HashFunc aHashFunc, CompareFunc aCompareFunc);
                                                /* Deallocate all of the
                                                  hash nodes and the cache
                                                  itself. */
void 
hash_delete (Cache_t theCache);
                                                /* Add the key/value pair
                                                  to the hash table.  If the
                                                  hash table reaches a 
                                                  level of fullnes then
                                                  it will be resized. 
                                                   
                                                  assert() if the key is 
                                                  already in the hash. */
void 
hash_add (Cache_t* theCache, void* aKey, void* aValue);
                                                /* Remove the key/value pair
                                                  from the hash table.  
                                                  assert() if the key isn't 
                                                  in the table. */
void 
hash_remove (Cache_t theCache, void* aKey);
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
CacheNode_t 
hash_next (Cache_t theCache, CacheNode_t aCacheNode);

                                               /* Used to return a value from 
						  a hash table using a given 
						  key.  */
void* 
hash_value_for_key (Cache_t theCache, void* aKey);


/************************************************

        Useful hashing functions.  
        
        Declared inline for your pleaseure. 
        
************************************************/

                                                /* Calculate a hash code by 
						   performing some 
						   manipulation of the key 
						   pointer. */
static inline u_int 
intHash(Cache_t theCache, void* aKey) {


  assert(sizeof (u_int) == sizeof (aKey));

  return ((u_int)aKey >> (sizeof(void*) - 1)) & theCache->mask ;
}

                                                /* Calculate a hash code by 
						   iterating over a NULL 
						   terminate string. */
static inline u_int 
strHash(Cache_t theCache, void* aKey) {

  u_int   ret = 0;
  u_int   ctr = 0;
        
        
  while(*(char*)aKey) {
    ret ^= *(char*)aKey++ << ctr;
    ctr = (ctr + 1) % sizeof(void*);
  }

  return ret & theCache->mask ;
}


/* Compare two integers. */
static inline int 
intCmp(void* k1, void* k2) {


        return !((int)k1 - (int)k2);
}


/* Compare two strings. */
static inline int 
strCmp(void* k1, void* k2) {


  return !strcmp( k1, k2 );
}


#ifdef __cplusplus
}
#endif

#endif
