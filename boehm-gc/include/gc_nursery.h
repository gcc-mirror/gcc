
/* 
 * Copyright (c) 1999 by Silicon Graphics.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

/*
 * THIS IMPLEMENTATION FOR THIS INTERFACE IS INCOMPLETE.
 * NONE OF THIS HAS BEEN TESTED.  DO NOT USE.
 *
 * Comments on the interface are appreciated, especially from
 * potential users of the interface.
 *
 * This is a Bartlett style copying collector for young objects.
 * We assume for now that all objects allocated through this
 * mechanism have pointers only in the first BITMAP_BITS words.
 * (On a 32-bit machine, BITMAP_BITS is 30.)
 * Objects allocated in this manner should be rarely referenced
 * by objects not allocated either through this interface, or through
 * the typed allocation interface.
 * If this interface is used, we assume that type information provided
 * through either this or the typed allocation interface is valid
 * in a stronger sense:
 *
 * 1) No pointers are stored in fields not marked as such.
 *    (Otherwise it is only necessary that objects referenced by
 *    fields marked as nonpointers are also reachable via another
 *    path.)
 * 2) Values stored in pointer fields are either not addresses in
 *    the heap, or they really are pointers.  In the latter case, it
 *    is acceptable to move the object they refer to, and to update
 *    the pointer.
 *
 * GC_free may not be invoked on objects allocated with GC_copying_malloc.
 *
 * No extra space is added to the end of objects allocated through this
 * interface.  If the client needs to maintain pointers past the
 * end, the size should be explicitly padded.
 * 
 * We assume that calls to this will usually be compiler generated.
 * Hence the interface is allowed to be a bit ugly in return for speed.
 */

#include "gc_copy_descr.h"

/* GC_copy_descr.h must define						*/
/* GC_SIZE_FROM_DESCRIPTOR(descr) and					*/
/* GC_BIT_MAP_FROM_DESCRIPTOR(descr).					*/
/* It may either be the GC supplied version of the header file, or a	*/
/* client specific one that derives the information from a client-	*/
/* specific type descriptor.						*/

typedef GC_PTR GC_copy_alloc_state;
				/* Current allocator state.	*/
				/* Multiple allocation states	*/
				/* may be used for concurrent	*/
				/* allocation, or to enhance	*/
				/* locality.			*/
				/* Should be treated as opaque.	*/

/* Allocate a memory block of size given in the descriptor, and with	*/
/* pointer layout given by the descriptor.  The resulting block may not	*/
/* be cleared, and should immediately be initialized by the client.	*/
/* (A concurrent GC may see an uninitialized pointer field.  If it	*/
/* points outside the nursery, that's fine.  If it points inside, it	*/
/* may retain an object, and be relocated.  But that's also fine, since	*/
/* the new value will be immediately overwritten.			*/
/* This variant acquires the allocation lock, and uses a default 	*/
/* global allocation state.						*/
GC_PTR GC_copying_malloc(GC_copy_descriptor);

/* A variant of the above that does no locking on the fast path,	*/
/* and passes an explicit pointer to an allocation state.		*/
/* The allocation state is updated.					*/
/* There will eventually need to be a macro or inline function version	*/
/* of this.								*/
GC_PTR GC_copying_malloc2(GC_copy_descriptor, GC_copy_alloc_state *);

/* Initialize an allocation state so that it can be used for 	*/
/* allocation.  This implicitly reserves a small section of the	*/
/* nursery for use with this allocator.				*/
void GC_init_copy_alloc_state(GC_copy_alloc_state *);
