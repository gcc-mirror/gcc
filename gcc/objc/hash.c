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
  $Header: /home/fsf/rms/c-runtime/dispatch/RCS/hash.c,v 0.15 1992/09/02 02:04:32 rms Exp rms $
  $Author: rms $
  $Date: 1992/09/02 02:04:32 $
  $Log: hash.c,v $
 * Revision 0.15  1992/09/02  02:04:32  rms
 * Changed some decls.
 *
 * Revision 0.14  1992/08/31  21:09:15  dglattin
 * minor documentation changes.
 *
 * Revision 0.13  1992/08/18  04:46:58  dglattin
 * Saving a working version before release.
 *
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
 * deleted hashIndex and moved it to hash-inline.h
 * converted hash_value_for_key to a inline and moved it to hash-inline.h.
 *
 * Revision 0.6  1991/11/21  22:27:06  dennisg
 * changed hash value calculation.
 * func name changed from hashValue to hashIndex.  the
 * func really calculated a index anyway.
 * changed hash func impl.  essentially it was calculating a hash value
 * from a hash value.  this is a implementation thing.
 *
 * Revision 0.5  1991/11/20  23:29:20  dennisg
 * converted hashIndex to a inline.
 *
 * Revision 0.4  1991/11/19  12:34:41  dennisg
 * bug in hash_delete.  It was using void* to obtain nodes to
 * pass to hash_remove.  The value passed to hash_removed is a
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
 

#include "config.h"
#include "hash.h"
#include "objc.h"
#include "objcP.h"
#include "objc-protoP.h"

#include "assert.h"


/* These two macros determine when a hash table is full and
   by how much it should be expanded respectively.
                                                  
   These equations are percentages.  */
#define FULLNESS(cache) \
   ((((cache)->size * 75) / 100) <= (cache)->used)
#define EXPANSION(cache) \
  ((cache)->size * 2)

cache_ptr 
hash_new (u_int size, hash_func_type hash_func, compare_func_type compare_func)
{
  cache_ptr cache;


  /* Pass me a value greater than 0 and a power of 2.  */
  assert (size);
  assert (!(size & (size - 1)));
  
  /* Allocate the cache structure.  calloc insures
     its initialization for default values.  */
  cache = (cache_ptr) calloc (1, sizeof (struct cache));
  assert (cache);
  
  /* Allocate the array of buckets for the cache.  
     calloc initializes all of the pointers to NULL.  */
  cache->node_table
    = (node_ptr *) calloc (size, sizeof (node_ptr));
  assert (cache->node_table);
  
  cache->size  = size;

  /* This should work for all processor architectures? */
  cache->mask = (size - 1);
	
  /* Store the hashing function so that codes can be computed.  */
  cache->hash_func = hash_func;

  /* Store the function that compares hash keys to 
     determine if they are equal.  */
  cache->compare_func = compare_func;

  return cache;
}


void 
hash_delete (cache_ptr cache)
{
  node_ptr node;


  /* Purge all key/value pairs from the table.  */
  while (node = hash_next (cache, NULL))
    hash_remove (cache, node->key);

  /* Release the array of nodes and the cache itself.  */
  free (cache->node_table);
  free (cache);
}


void 
hash_add (cache_ptr *cachep, void *key, void *value)
{
  u_int indx = (*(*cachep)->hash_func)(*cachep, key);
  node_ptr node = (node_ptr) calloc (1, sizeof (struct cache_node));


  assert (node);
  
  /* Initialize the new node.  */
  node->key    = key;
  node->value  = value;
  node->next  = (*(*cachep)->node_table)[indx];
  
  /* Debugging.
     Check the list for another key.  */
#ifdef DEBUG
  { node_ptr node1 = (*(*cachep)->node_table)[indx];
    
    while (node1) {
    
      assert (node1->key != key);
      node1 = node1->next;
    }
  }
#endif

  /* Install the node as the first element on the list.  */
  (*(*cachep)->node_table)[indx] = node;

  /* Bump the number of entries in the cache.  */
  ++(*cachep)->used;
  
  /* Check the hash table's fullness.   We're going
     to expand if it is above the fullness level.  */
  if (FULLNESS (*cachep)) {
    
    /* The hash table has reached its fullness level.  Time to
       expand it. 
       
       I'm using a slow method here but is built on other
       primitive functions thereby increasing its 
       correctness.  */
    node_ptr node1 = NULL;
    cache_ptr new = hash_new (EXPANSION (*cachep), 
			      (*cachep)->hash_func, 
			      (*cachep)->compare_func);

    DEBUG_PRINTF (stderr, "Expanding cache %#x from %d to %d\n",
		  *cachep, (*cachep)->size, new->size);
      
    /* Copy the nodes from the first hash table to the new one.  */
    while (node1 = hash_next (*cachep, node1))
      hash_add (&new, node1->key, node1->value);

    /* Trash the old cache.  */
    hash_delete (*cachep);
    
    /* Return a pointer to the new hash table.  */
    *cachep = new;
  }
}


void 
hash_remove (cache_ptr cache, void *key)
{
  u_int indx = (*cache->hash_func)(cache, key);
  node_ptr node = (*cache->node_table)[indx];
  
  
  /* We assume there is an entry in the table.  Error if it is not.  */
  assert (node);
  
  /* Special case.  First element is the key/value pair to be removed.  */
  if ((*cache->compare_func)(node->key, key)) {
    (*cache->node_table)[indx] = node->next;
    free (node);
  } else {

    /* Otherwise, find the hash entry.  */
    node_ptr prev = node;
    BOOL        removed = NO;
    
    do {
    
      if ((*cache->compare_func)(node->key, key)) {
        prev->next = node->next, removed = YES;
        free (node);
      } else
        prev = node, node = node->next;
    } while (!removed && node);
    assert (removed);
  }
  
  /* Decrement the number of entries in the hash table.  */
  --cache->used;
}


node_ptr 
hash_next (cache_ptr cache, node_ptr node)
{
  /* If the scan is being started then reset the last node 
     visitied pointer and bucket index.  */
  if (!node)
    cache->last_bucket  = 0;
  
  /* If there is a node visited last then check for another 
     entry in the same bucket;  Otherwise step to the next bucket.  */
  if (node) {
    if (node->next)
      /* There is a node which follows the last node 
	 returned.  Step to that node and retun it.  */
      return node->next;
    else
      ++cache->last_bucket;
  }

  /* If the list isn't exhausted then search the buckets for 
     other nodes.  */
  if (cache->last_bucket < cache->size) {
    /*  Scan the remainder of the buckets looking for an entry
	at the head of the list.  Return the first item found.  */
    while (cache->last_bucket < cache->size)
      if ((*cache->node_table)[cache->last_bucket])
        return (*cache->node_table)[cache->last_bucket];
      else
        ++cache->last_bucket;
  
    /* No further nodes were found in the hash table.  */
    return NULL;
  } else
    return NULL;
}


/* 
 * Given key, return its value.  Return NULL if the
 * key/value pair isn't in the hash. 
 */
void *
hash_value_for_key (cache_ptr cache, void *key)
{
  node_ptr node
    =  (*cache->node_table)[(*cache->hash_func)(cache, key)];
  void *retval = NULL;
  

  if (node)
    do {
      if ((*cache->compare_func)(node->key, key))
        retval = node->value;
      else
        node = node->next;
    } while (!retval && node);
  
  return retval;
}
