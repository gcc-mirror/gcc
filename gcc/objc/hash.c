/* -*-c-*-
 * This file contains the hashing implementation.
 *
  $Header$
  $Author$
  $Date$
  $Log$
*/
 

#include  <hash.h>
#include  <ObjC.h>

#include  <assert.h>
#include  <libc.h>
#include  <math.h>


                                                /* Local forward decl. */
  u_int hashValue( Cache_t, void* );


Cache_t hash_new( u_int numberOfBuckets ) {

  Cache_t retCache;
  int     i;


  assert( numberOfBuckets );

                                                /* Allocate the cache 
                                                  structure.  calloc() insures
                                                  its initialization for
                                                  default values. */
  retCache = calloc( 1, sizeof( Cache ));
  assert( retCache );
  
                                                /* Allocate the array of 
                                                  buckets for the cache.  
                                                  calloc() initializes all of 
                                                  the pointers to NULL. */
  retCache->theNodeTable = calloc( numberOfBuckets, sizeof( CacheNode_t ));
  assert( retCache->theNodeTable );
  
  retCache->numberOfBuckets = numberOfBuckets;

                                                /* Calculate the number of 
                                                  bits required to represent 
                                                  the hash mask. */
  retCache->numberOfMaskBits = 
    ceil( log( retCache->numberOfBuckets ) / log( 2 ));

                                                /* Form a bit mask for the 
                                                  hash. */
  for( i = 0; i < retCache->numberOfMaskBits; ++i )
    retCache->mask = ( retCache->mask << 1 ) | 0x01 ;

  assert( retCache->numberOfMaskBits );
  assert( retCache->mask );

  return retCache;
}


void hash_delete( Cache_t theCache ) {

  void* aNode;
  

                                                /* Purge all key/value pairs 
                                                  from the table. */
  while( aNode = hash_next( theCache, NULL ))
    hash_remove( theCache, aNode );

                                                /* Release the array of nodes 
                                                  and the cache itself. */
  free( theCache->theNodeTable );
  free( theCache );
}


void hash_add( Cache_t theCache, void* aKey, void* aValue ) {

  u_int       indx = hashValue( theCache, aKey );
  CacheNode_t aCacheNode = calloc( 1, sizeof( CacheNode ));


  assert( aCacheNode );
  
                                                /* Initialize the new node. */
  aCacheNode->theKey    = aKey;
  aCacheNode->theValue  = aValue;
  aCacheNode->nextNode  = ( *theCache->theNodeTable )[ indx ];
  
                                                /* Debugging.
                                                
                                                  Check the list for another 
                                                  key. */
#ifdef DEBUG
    { CacheNode_t checkHashNode = ( *theCache->theNodeTable )[ indx ];
    
      while( checkHashNode ) {
    
        assert( checkHashNode->theKey != aKey );
        checkHashNode = checkHashNode->nextNode;
      }
    }

                                                /* Install the node as the
                                                  first element on the list. */
  ( *theCache->theNodeTable )[ indx ] = aCacheNode;

#endif
}


void hash_remove( Cache_t theCache, void* aKey ) {

  u_int       indx = hashValue( theCache, aKey );
  CacheNode_t aCacheNode = ( *theCache->theNodeTable )[ indx ];
  
  
                                                /* We assume there is an entry 
                                                  in the table.  Error if it 
                                                  is not. */
  assert( aCacheNode );
  
                                                /* Special case.  First element 
                                                  is the key/value pair to be 
                                                  removed. */
  if( aCacheNode->theKey == aKey ) {
    ( *theCache->theNodeTable )[ indx ] = aCacheNode->nextNode;
    free( aCacheNode );
  } else {
                                                /* Otherwise, find the hash 
                                                  entry. */
    CacheNode_t prevHashNode = aCacheNode;
    BOOL        removed = NO;
    
    do {
    
      if( aCacheNode->theKey == aKey ) {
        prevHashNode->nextNode = aCacheNode->nextNode, removed = YES;
        free( aCacheNode );
      } else
        prevHashNode = aCacheNode, aCacheNode = aCacheNode->nextNode;
    } while( !removed && aCacheNode );
    assert( removed );
  }
}


void* hash_value_for_key( Cache_t theCache, void* aKey ) {

  u_int       indx = hashValue( theCache, aKey );
  CacheNode_t aCacheNode = ( *theCache->theNodeTable )[ indx ];
  void*       retVal = NULL;
  

  if( aCacheNode ) {
    BOOL  found = NO;
  
    do {
      if( aCacheNode->theKey == aKey )
        retVal = aCacheNode->theValue, found = YES;
      else
        aCacheNode = aCacheNode->nextNode;
    } while( !found && aCacheNode );
  }
  
  return retVal;
}


CacheNode_t hash_next( Cache_t theCache, CacheNode_t aCacheNode ) {

  CacheNode_t theCacheNode = aCacheNode;
  
  
                                                /* If the scan is being started
                                                  then reset the last node 
                                                  visitied pointer and bucket 
                                                  index. */
  if( !theCacheNode )
    theCache->lastBucket  = 0;
  
                                                /* If there is a node visited
                                                  last then check for another 
                                                  entry in the same bucket; 
                                                  Otherwise step to the next 
                                                  bucket. */
  if( theCacheNode )
    if( theCacheNode->nextNode )
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
  if( theCache->lastBucket < theCache->numberOfBuckets ) {
                                                /*  Scan the remainder of the 
                                                  buckets looking for an entry
                                                  at the head of the list.  
                                                  Return the first item 
                                                  found. */
    while( theCache->lastBucket < theCache->numberOfBuckets )
      if(( *theCache->theNodeTable )[ theCache->lastBucket ])
        return ( *theCache->theNodeTable )[ theCache->lastBucket ];
      else
        ++theCache->lastBucket;
  
                                                /* No further nodes were found
                                                  in the hash table. */
    return NULL;
  } else
    return NULL;
}


u_int hashValue( Cache_t theCache, void* aKey ) {

  u_int hash = 0;
  int   i;
  
  
  assert( theCache->numberOfMaskBits );
  for( i = 0; i < ( sizeof( aKey ) * 8 ); i += theCache->numberOfMaskBits )
    hash ^= (( u_int )aKey ) >> i ;

  return ( hash & theCache->mask ) % theCache->numberOfBuckets;
}

