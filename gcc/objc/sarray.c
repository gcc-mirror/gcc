/* Sparse Arrays for Objective C dispatch tables
   Copyright (C) 1993 Free Software Foundation, Inc.

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

#include "objc/sarray.h"
#include <stdio.h>
#include "assert.h"

int nbuckets = 0;
int nindices = 0;
int narrays = 0;
int idxsize = 0;

#ifdef OBJC_SPARSE2
const char* __objc_sparse2_id = "2 level sparse indices";
#endif

#ifdef OBJC_SPARSE3
const char* __objc_sparse3_id = "3 level sparse indices";
#endif

#ifdef __alpha__
const void *memcpy (void*, const void*, size_t);
void free (const void*);
#endif

void
sarray_at_put(struct sarray* array, sidx index, void* element)
{
#ifdef OBJC_SPARSE3
  struct sindex** the_index;
#endif
  struct sbucket** the_bucket;
#ifdef OBJC_SPARSE3
  size_t ioffset;
#endif
  size_t boffset;
  size_t eoffset;
#ifdef PRECOMPUTE_SELECTORS
  union sofftype xx; 
  xx.idx = index;
#ifdef OBJC_SPARSE3
  ioffset = xx.off.ioffset;
#endif
  boffset = xx.off.boffset;
  eoffset = xx.off.eoffset;
#else /* not PRECOMPUTE_SELECTORS */
#ifdef OBJC_SPARSE3
  ioffset = index/INDEX_CAPACITY;
  boffset = (index/BUCKET_SIZE)%INDEX_SIZE;
  eoffset = index%BUCKET_SIZE;
#else
  boffset = index/BUCKET_SIZE;
  eoffset = index%BUCKET_SIZE;
#endif
#endif /* not PRECOMPUTE_SELECTORS */

  assert(soffset_decode(index) < array->capacity); /* Range check */

#ifdef OBJC_SPARSE3
  the_index = &(array->indices[ioffset]);
  the_bucket = &((*the_index)->buckets[boffset]);
#else
  the_bucket = &(array->buckets[boffset]);
#endif
  
  if ((*the_bucket)->elems[eoffset] == element)
    return;		/* great! we just avoided a lazy copy */

#ifdef OBJC_SPARSE3

  /* First, perform lazy copy/allocation of index if needed */

  if ((*the_index) == array->empty_index) {

    /* The index was previously empty, allocate a new */
    *the_index = (struct sindex*)__objc_xmalloc(sizeof(struct sindex));
    memcpy(*the_index, array->empty_index, sizeof(struct sindex));
    (*the_index)->version = array->version;
    the_bucket = &((*the_index)->buckets[boffset]);
    nindices += 1;
    
  } else if ((*the_index)->version != array->version) {

    /* This index must be lazy copied */
    struct sindex* old_index = *the_index;
    *the_index = (struct sindex*)__objc_xmalloc(sizeof(struct sindex));
    memcpy( *the_index,old_index, sizeof(struct sindex));
    (*the_index)->version = array->version;
    the_bucket = &((*the_index)->buckets[boffset]);
    nindices += 1;
    
  }

#endif /* OBJC_SPARSE3 */

  /* next, perform lazy allocation/copy of the bucket if needed */

  if ((*the_bucket) == array->empty_bucket) {

    /* The bucket was previously empty (or something like that), */
    /* allocate a new.  This is the effect of `lazy' allocation */  
    *the_bucket = (struct sbucket*)__objc_xmalloc(sizeof(struct sbucket));
    memcpy((void *) *the_bucket, (const void*)array->empty_bucket, sizeof(struct sbucket));
    (*the_bucket)->version = array->version;
    nbuckets += 1;

  } else if ((*the_bucket)->version != array->version) {

    /* Perform lazy copy. */
    struct sbucket* old_bucket = *the_bucket;
    *the_bucket = (struct sbucket*)__objc_xmalloc(sizeof(struct sbucket));
    memcpy( *the_bucket,old_bucket, sizeof(struct sbucket));
    (*the_bucket)->version = array->version;
    nbuckets += 1;

  }
  (*the_bucket)->elems[eoffset] = element;
}

void
sarray_at_put_safe(struct sarray* array, sidx index, void* element)
{
  if(soffset_decode(index) >= array->capacity)
    sarray_realloc(array, soffset_decode(index)+1);
  sarray_at_put(array, index, element);
}

struct sarray* 
sarray_new (int size, void* default_element)
{
#ifdef OBJC_SPARSE3
  size_t num_indices = ((size-1)/(INDEX_CAPACITY))+1;
#else /* OBJC_SPARSE2 */
  size_t num_indices = ((size-1)/BUCKET_SIZE)+1;
#endif
  int counter;
  struct sarray* arr;

  assert(size > 0);

  /* Allocate core array */
  arr = (struct sarray*) __objc_xmalloc(sizeof(struct sarray));
  arr->version = 0;
  narrays  += 1;
  
  /* Initialize members */
#ifdef OBJC_SPARSE3
  arr->capacity = num_indices*INDEX_CAPACITY;
  arr->indices = (struct sindex**) 
    __objc_xmalloc(sizeof(struct sindex*)*num_indices);
  idxsize  += num_indices;

  arr->empty_index = (struct sindex*) __objc_xmalloc(sizeof(struct sindex));
  arr->empty_index->version = 0;
  nindices += 1;

#else /* OBJC_SPARSE2 */
  arr->capacity = num_indices*BUCKET_SIZE;
  arr->buckets = (struct sbucket**) 
    __objc_xmalloc(sizeof(struct sbucket*)*num_indices);
  idxsize  += num_indices;

#endif

  arr->empty_bucket = (struct sbucket*) __objc_xmalloc(sizeof(struct sbucket));
  arr->empty_bucket->version = 0;
  nbuckets += 1;

  arr->ref_count = 1;
  arr->is_copy_of = (struct sarray*)0;
  
  for (counter=0; counter<BUCKET_SIZE; counter++)
    arr->empty_bucket->elems[counter] = default_element;

#ifdef OBJC_SPARSE3
  for (counter=0; counter<INDEX_SIZE; counter++)
    arr->empty_index->buckets[counter] = arr->empty_bucket;

  for (counter=0; counter<num_indices; counter++)
    arr->indices[counter] = arr->empty_index;

#else /* OBJC_SPARSE2 */

  for (counter=0; counter<num_indices; counter++)
    arr->buckets[counter] = arr->empty_bucket;

#endif

  return arr;
}


/* Reallocate the sparse array to hold `newsize' entries */

void 
sarray_realloc(struct sarray* array, int newsize)
{
#ifdef OBJC_SPARSE3
  int old_max_index = (array->capacity-1)/INDEX_CAPACITY;
  int new_max_index = ((newsize-1)/INDEX_CAPACITY);
  int rounded_size = (new_max_index+1)*INDEX_CAPACITY;

#else /* OBJC_SPARSE2 */
  int old_max_index = (array->capacity-1)/BUCKET_SIZE;
  int new_max_index = ((newsize-1)/BUCKET_SIZE);
  int rounded_size = (new_max_index+1)*BUCKET_SIZE;

#endif

  int counter;

  assert(newsize > 0);

  /* The size is the same, just ignore the request */
  if(rounded_size == array->capacity)
    return;

  assert(array->ref_count == 1);	/* stop if lazy copied... */

  if(rounded_size < array->capacity) 
    {
      /* update capacity */
      array->capacity = rounded_size;

      /* free buckets above new_max_index */
      for(counter = old_max_index; counter > new_max_index; counter-- ) {
#ifdef OBJC_SPARSE3
	struct sindex* idx = array->indices[counter];
	if((idx != array->empty_index) && (idx->version == array->version)) {
	  int c2; 
	  for(c2=0; c2<INDEX_SIZE; c2++) {
	    struct sbucket* bkt = idx->buckets[c2];
	    if((bkt != array->empty_bucket) && (bkt->version == array->version))
	      {
		free(bkt);
		nbuckets -= 1;
	      }
	  }
	  free(idx);
	  nindices -= 1;
	}
#else /* OBJC_SPARSE2 */
	struct sbucket* bkt = array->buckets[counter];
	if ((bkt != array->empty_bucket) && (bkt->version == array->version))
	  {
	    free(bkt);
	    nbuckets -= 1;
	  }
#endif
      }
	  
#ifdef OBJC_SPARSE3
      /* realloc to free the space above new_max_index */
      array->indices = (struct sindex**)
	__objc_xrealloc(array->indices, 
			(new_max_index+1)*sizeof(struct sindex*));
#else /* OBJC_SPARSE2 */
      array->buckets = (struct sbucket**)
	__objc_xrealloc(array->buckets, 
			(new_max_index+1)*sizeof(struct sbucket*));
#endif      
      idxsize -= (old_max_index-new_max_index);

      return;
    }

  /* We are asked to extend the array -- reallocate the bucket table, */
  /* and insert empty_bucket in newly allocated places. */
  if(rounded_size > array->capacity) 
    {
      /* update capacity */
      array->capacity = rounded_size;

#ifdef OBJC_SPARSE3
      /* realloc to make room in table above old_max_index */
      array->indices = (struct sindex**)
	__objc_xrealloc(array->indices, 
			(new_max_index+1)*sizeof(struct sindex*));

      /* reset entries above old_max_index to empty_bucket */
      for(counter = old_max_index+1; counter <= new_max_index; counter++)
	array->indices[counter] = array->empty_index;

#else /* OBJC_SPARSE2 */

      /* realloc to make room in table above old_max_index */
      array->buckets = (struct sbucket**)
	__objc_xrealloc(array->buckets, 
			(new_max_index+1)*sizeof(struct sbucket*));

      /* reset entries above old_max_index to empty_bucket */
      for(counter = old_max_index+1; counter <= new_max_index; counter++)
	array->buckets[counter] = array->empty_bucket;

#endif
      idxsize += (new_max_index-old_max_index);
      return;
    }
}


/* Free a sparse array allocated with sarray_new */

void 
sarray_free(struct sarray* array) {
#ifdef OBJC_SPARSE3
  size_t old_max_index = (array->capacity-1)/INDEX_CAPACITY;
#else
  size_t old_max_index = (array->capacity-1)/BUCKET_SIZE;
#endif
  int counter = 0;

  assert(array->ref_count != 0);	/* Freed multiple times!!! */

  if(--(array->ref_count) != 0)	/* There exists copies of me */
    return;

  if((array->is_copy_of) && ((array->is_copy_of->ref_count - 1) == 0))
    sarray_free(array->is_copy_of);

  /* Free all entries that do not point to empty_bucket */
  for(counter = 0; counter <= old_max_index; counter++ ) {
#ifdef OBJC_SPARSE3
    struct sindex* idx = array->indices[counter];
    if((idx != array->empty_index) && (idx->version == array->version)) {
      int c2; 
      for(c2=0; c2<INDEX_SIZE; c2++) {
	struct sbucket* bkt = idx->buckets[c2];
	if((bkt != array->empty_bucket) && (bkt->version == array->version))
	  {
	    free(bkt);
	    nbuckets -= 1;
	  }
      }
      free(idx);
      nindices -= 1;
    }
#else /* OBJC_SPARSE2 */
    struct sbucket* bkt = array->buckets[counter];
    if ((bkt != array->empty_bucket) && (bkt->version == array->version))
      {
	free(bkt);
	nbuckets -= 1;
      }
#endif
  }
	
#ifdef OBJC_SPARSE3  
  /* free empty_index */
  if(array->empty_index->version == array->version) {
    free(array->empty_index);
    nindices -= 1;
  }
#endif

  /* free empty_bucket */
  if(array->empty_bucket->version == array->version) {
    free(array->empty_bucket);
    nbuckets -= 1;
  }

#ifdef OBJC_SPARSE3
  /* free bucket table */
  free(array->indices);
  idxsize -= (old_max_index+1);

#else
  /* free bucket table */
  free(array->buckets);
  idxsize -= (old_max_index+1);

#endif

  /* free array */
  free(array);
  narrays -= 1;
}

/* This is a lazy copy.  Only the core of the structure is actually */
/* copied.   */

struct sarray* 
sarray_lazy_copy(struct sarray* oarr)
{
#ifdef OBJC_SPARSE3
  size_t num_indices = ((oarr->capacity-1)/INDEX_CAPACITY)+1;
#else /* OBJC_SPARSE2 */
  size_t num_indices = ((oarr->capacity-1)/BUCKET_SIZE)+1;
#endif
  struct sarray* arr;

  /* Allocate core array */
  arr = (struct sarray*) __objc_xmalloc(sizeof(struct sarray));
  memcpy( arr,oarr, sizeof(struct sarray));
  arr->version = oarr->version + 1;
  arr->is_copy_of = oarr;
  oarr->ref_count += 1;
  arr->ref_count = 1;
  
#ifdef OBJC_SPARSE3
  /* Copy bucket table */
  arr->indices = (struct sindex**) 
    __objc_xmalloc(sizeof(struct sindex*)*num_indices);
  memcpy( arr->indices,oarr->indices, 
	sizeof(struct sindex*)*num_indices);
#else 
  /* Copy bucket table */
  arr->buckets = (struct sbucket**) 
    __objc_xmalloc(sizeof(struct sbucket*)*num_indices);
  memcpy( arr->buckets,oarr->buckets, 
	sizeof(struct sbucket*)*num_indices);
#endif

  idxsize += num_indices;
  narrays += 1;

  return arr;
}
