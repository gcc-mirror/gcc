/* 
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright 1996 Silicon Graphics.  All rights reserved.
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
 * Some simple primitives for allocation with explicit type information.
 * Facilities for dynamic type inference may be added later.
 * Should be used only for extremely performance critical applications,
 * or if conservative collector leakage is otherwise a problem (unlikely).
 * Note that this is implemented completely separately from the rest
 * of the collector, and is not linked in unless referenced.
 * This does not currently support GC_DEBUG in any interesting way.
 */
/* Boehm, May 19, 1994 2:13 pm PDT */

#ifndef _GC_TYPED_H
# define _GC_TYPED_H
# ifndef _GC_H
#   include "gc.h"
# endif

typedef GC_word * GC_bitmap;
	/* The least significant bit of the first word is one if	*/
	/* the first word in the object may be a pointer.		*/
	
# define GC_get_bit(bm, index) \
		(((bm)[divWORDSZ(index)] >> modWORDSZ(index)) & 1)
# define GC_set_bit(bm, index) \
		(bm)[divWORDSZ(index)] |= (word)1 << modWORDSZ(index)

typedef GC_word GC_descr;

GC_API GC_descr GC_make_descriptor GC_PROTO((GC_bitmap bm, size_t len));
		/* Return a type descriptor for the object whose layout	*/
		/* is described by the argument.			*/
		/* The least significant bit of the first word is one	*/
		/* if the first word in the object may be a pointer.	*/
		/* The second argument specifies the number of		*/
		/* meaningful bits in the bitmap.  The actual object 	*/
		/* may be larger (but not smaller).  Any additional	*/
		/* words in the object are assumed not to contain 	*/
		/* pointers.						*/
		/* Returns a conservative approximation in the		*/
		/* (unlikely) case of insufficient memory to build	*/
		/* the descriptor.  Calls to GC_make_descriptor		*/
		/* may consume some amount of a finite resource.  This	*/
		/* is intended to be called once per type, not once	*/
		/* per allocation.					*/

GC_API GC_PTR GC_malloc_explicitly_typed
			GC_PROTO((size_t size_in_bytes, GC_descr d));
		/* Allocate an object whose layout is described by d.	*/
		/* The resulting object MAY NOT BE PASSED TO REALLOC.	*/
		/* The returned object is cleared.			*/

GC_API GC_PTR GC_malloc_explicitly_typed_ignore_off_page
                        GC_PROTO((size_t size_in_bytes, GC_descr d));
		
GC_API GC_PTR GC_calloc_explicitly_typed
			GC_PROTO((size_t nelements,
  				  size_t element_size_in_bytes,
  				  GC_descr d));
  	/* Allocate an array of nelements elements, each of the	*/
  	/* given size, and with the given descriptor.		*/
  	/* The elemnt size must be a multiple of the byte	*/
  	/* alignment required for pointers.  E.g. on a 32-bit	*/
  	/* machine with 16-bit aligned pointers, size_in_bytes	*/
  	/* must be a multiple of 2.				*/
	/* Returned object is cleared.				*/

#ifdef GC_DEBUG
#   define GC_MALLOC_EXPLICTLY_TYPED(bytes, d) GC_MALLOC(bytes)
#   define GC_CALLOC_EXPLICTLY_TYPED(n, bytes, d) GC_MALLOC(n*bytes)
#else
#  define GC_MALLOC_EXPLICTLY_TYPED(bytes, d) \
	GC_malloc_explicitly_typed(bytes, d)
#  define GC_CALLOC_EXPLICTLY_TYPED(n, bytes, d) \
	GC_calloc_explicitly_typed(n, bytes, d)
#endif /* !GC_DEBUG */


#endif /* _GC_TYPED_H */

