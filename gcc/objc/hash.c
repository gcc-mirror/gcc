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
  $Header: /usr/user/dennis_glatting/ObjC/c-runtime/dispatch.common/RCS/hash.c,v 0.12 1992/04/13 11:43:08 dennisg Exp dennisg $
  $Author: dennisg $
  $Date: 1992/04/13 11:43:08 $
  $Log: hash.c,v $
 * Revision 0.12  1992/04/13  11:43:08  dennisg
 * Check in after array version of run-time works.
 * Expect more changes as hash version and other changes are made.
 *
 * Revision 0.11  1992/01/03  02:55:03  dennisg
 * modified to handle new initialization scheme.
 * fixed code structure.
 *
 * Revision 0.10  1991/12/10  12:05:28  dennisg
 * Cleaned up file format for a distribution.
 *
 * Revision 0.9  1991/12/03  02:01:23  dennisg
 * fixed assert macro.
 * added memory allocation adjustment macro for hash size allocation.
 *
 * Revision 0.8  1991/11/24  01:20:02  dennisg
 * changed shorts back to ints.
 * the efficiency gained didn't out weight the grossness of the code.
 *
 * Revision 0.7  1991/11/23  22:18:29  dennisg
 * deleted hashIndex() and moved it to hash-inline.h
 * converted hash_value_for_key () to a inline and moved it to hash-inline.h.
 *
 * Revision 0.6  1991/11/21  22:27:06  dennisg
 * changed hash value calculation.
 * func name changed from hashValue () to hashIndex().  the
 * func really calculated a index anyway.
 * changed hash func impl.  essentially it was calculating a hash value
 * from a hash value.  this is a implementation thing.
 *
 * Revision 0.5  1991/11/20  23:29:20  dennisg
 * converted hashIndex() to a inline.
 *
 * Revision 0.4  1991/11/19  12:34:41  dennisg
 * bug in hash_delete ().  It was using void* to obtain nodes to
 * pass to hash_remove ().  The value passed to hash_removed () is a
 * entry from the node structure rather than the node itself.  Using
 * void* removed compiler checking.
 * Modified to implement cache expansion.
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
 

#include  <hash.h>
#include  <objc.h>
#include  <objcP.h>
#include  <objc-protoP.h>

#include  <assert.h>
#include  <math.h>
#include  <stdio.h>
#include  <stdlib.h>


                                                /* These two macros determine
                                                  when a hash table is full and
                                                  by how much it should be 
                                                  expanded respectively.
                                                  
                                                  These equations are 
                                                  percentages. */
#define FULLNESS(cache) \
   ((((cache)->sizeOfHash * 75  ) / 100) <= (cache)->entriesInHash)
#define EXPANSION(cache) \
  ((cache)->sizeOfHash * 2 )

Cache_t 
hash_new (u_int sizeOfHash, HashFunc aHashFunc, CompareFunc aCompareFunc) {

  Cache_t retCache;
  

																								/* Pass me a value greater
																									than 0 and a power of 2. */
  assert(sizeOfHash);
	assert( !(sizeOfHash & (sizeOfHash - 1)));
  
                                                /* Allocate the cache 
                                                  structure.  calloc () insures
                                                  its initialization for
                                                  default values. */
  retCache = calloc (1, sizeof (Cache));
  assert(retCache);
  
                                                /* Allocate the array of 
                                                  buckets for the cache.  
                                                  calloc() initializes all of 
                                                  the pointers to NULL. */
  retCache->theNodeTable = calloc (sizeOfHash, sizeof (CacheNode_t));
  assert(retCache->theNodeTable);
  
  retCache->sizeOfHash  = sizeOfHash;

																								/* This should work for all
																									processor architectures? */
	retCache->mask = ( sizeOfHash - 1 );
	
																								/* Store the hashing function
																									so that codes can be 
																									computed. */
	retCache->hashFunc = aHashFunc;

																								/* Store the function that
																									compares hash keys to 
																									determine if they are 
																									equal. */
	retCache->compareFunc = aCompareFunc;

  return retCache;
}


void 
hash_delete (Cache_t theCache) {

  CacheNode_t aNode;
  

                                               /* Purge all key/value pairs 
                                                  from the table. */
  while (aNode = hash_next (theCache, NULL))
    hash_remove (theCache, aNode->theKey);

                                                /* Release the array of nodes 
                                                  and the cache itself. */
  free (theCache->theNodeTable);
  free (theCache);
}


void 
hash_add (Cache_t* theCache, void* aKey, void* aValue) {

  u_int       indx = (* (*theCache)->hashFunc)(*theCache, aKey);
  CacheNode_t aCacheNode = calloc (1, sizeof (CacheNode));


  assert(aCacheNode);
  
                                                /* Initialize the new node. */
  aCacheNode->theKey    = aKey;
  aCacheNode->theValue  = aValue;
  aCacheNode->nextNode  = (* (*theCache)->theNodeTable)[ indx ];
  
                                                /* Debugging.
                                                
                                                  Check the list for another 
                                                  key. */
#ifdef DEBUG
    { CacheNode_t checkHashNode = (* (*theCache)->theNodeTable)[ indx ];
    
      while (checkHashNode) {
    
        assert(checkHashNode->theKey != aKey);
        checkHashNode = checkHashNode->nextNode;
      }
    }
#endif

                                                /* Install the node as the
                                                  first element on the list. */
  (* (*theCache)->theNodeTable)[ indx ] = aCacheNode;

                                                /* Bump the number of entries
                                                  in the cache. */
  ++ (*theCache)->entriesInHash;
  
                                                /* Check the hash table's
                                                  fullness.   We're going
                                                  to expand if it is above
                                                  the fullness level. */
  if (FULLNESS (*theCache)) {
                                                /* The hash table has reached
                                                  its fullness level.  Time to
                                                  expand it. 
                                                  
                                                  I'm using a slow method 
                                                  here but is built on other
                                                  primitive functions thereby
                                                  increasing its 
                                                  correctness. */
    CacheNode_t aNode = NULL;
    Cache_t     newCache = hash_new (EXPANSION (*theCache), 
																		 (*theCache)->hashFunc, 
																		 (*theCache)->compareFunc);

    DEBUG_PRINTF (stderr, "Expanding cache %#x from %d to %d\n",
      *theCache, (*theCache)->sizeOfHash, newCache->sizeOfHash);
      
                                                /* Copy the nodes from the
                                                  first hash table to the
                                                  new one. */
    while (aNode = hash_next (*theCache, aNode))
      hash_add (&newCache, aNode->theKey, aNode->theValue);

                                                /* Trash the old cache. */
    hash_delete (*theCache);
    
                                                /* Return a pointer to the new
                                                  hash table. */
    *theCache = newCache;
  }
}


void 
hash_remove (Cache_t theCache, void* aKey) {

  u_int       indx = (*theCache->hashFunc)(theCache, aKey);
  CacheNode_t aCacheNode = (*theCache->theNodeTable)[ indx ];
  
  
                                                /* We assume there is an entry 
                                                  in the table.  Error if it 
                                                  is not. */
  assert(aCacheNode);
  
                                                /* Special case.  First element 
                                                  is the key/value pair to be 
                                                  removed. */
  if ((*theCache->compareFunc)(aCacheNode->theKey, aKey)) {
    (*theCache->theNodeTable)[ indx ] = aCacheNode->nextNode;
    free (aCacheNode);
  } else {
                                                /* Otherwise, find the hash 
                                                  entry. */
    CacheNode_t prevHashNode = aCacheNode;
    BOOL        removed = NO;
    
    do {
    
      if ((*theCache->compareFunc)(aCacheNode->theKey, aKey)) {
        prevHashNode->nextNode = aCacheNode->nextNode, removed = YES;
        free (aCacheNode);
      } else
        prevHashNode = aCacheNode, aCacheNode = aCacheNode->nextNode;
    } while (!removed && aCacheNode);
    assert(removed);
  }
  
                                                /* Decrement the number of
                                                  entries in the hash table. */
  --theCache->entriesInHash;
}


CacheNode_t 
hash_next (Cache_t theCache, CacheNode_t aCacheNode) {

  CacheNode_t theCacheNode = aCacheNode;
  
  
                                                /* If the scan is being started
                                                  then reset the last node 
                                                  visitied pointer and bucket 
                                                  index. */
  if (!theCacheNode)
    theCache->lastBucket  = 0;
  
                                                /* If there is a node visited
                                                  last then check for another 
                                                  entry in the same bucket; 
                                                  Otherwise step to the next 
                                                  bucket. */
  if (theCacheNode)
    if (theCacheNode->nextNode)
                                                /* There is a node which 
                                                  follows the last node 
                                                  returned.  Step to that node 
                                                  and retun it. */
      return theCacheNode->nextNode;
    else
      ++theCache->lastBucket;

                                                /* If the list isn't exhausted 
                                                  then search the buckets for 
                                                  other nodes. */
  if (theCache->lastBucket < theCache->sizeOfHash) {
                                                /*  Scan the remainder of the 
                                                  buckets looking for an entry
                                                  at the head of the list.  
                                                  Return the first item 
                                                  found. */
    while (theCache->lastBucket < theCache->sizeOfHash)
      if ((*theCache->theNodeTable)[ theCache->lastBucket ])
        return (*theCache->theNodeTable)[ theCache->lastBucket ];
      else
        ++theCache->lastBucket;
  
                                                /* No further nodes were found
                                                  in the hash table. */
    return NULL;
  } else
    return NULL;
}


                                                /* Given key, return its 
                                                  value.  Return NULL if the
                                                  key/value pair isn't in
                                                  the hash. */
void* 
hash_value_for_key (Cache_t theCache, void* aKey) {

  CacheNode_t aCacheNode = 
              (*theCache->theNodeTable)[(*theCache->hashFunc)(theCache, aKey)];
  void*       retVal = NULL;
  

  if (aCacheNode)
    do {
      if ((*theCache->compareFunc)(aCacheNode->theKey, aKey))
        retVal = aCacheNode->theValue;
      else
        aCacheNode = aCacheNode->nextNode;
    } while (!retVal && aCacheNode);
  
  return retVal;
}
