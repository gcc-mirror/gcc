/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996 by Silicon Graphics.  All rights reserved.
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
 * These are extra allocation routines which are likely to be less
 * frequently used than those in malloc.c.  They are separate in the
 * hope that the .o file will be excluded from statically linked
 * executables.  We should probably break this up further.
 */

#include <stdio.h>
#include "gc_priv.h"

extern ptr_t GC_clear_stack();  /* in misc.c, behaves like identity */
void GC_extend_size_map();      /* in misc.c. */
GC_bool GC_alloc_reclaim_list();	/* in malloc.c */

/* Some externally visible but unadvertised variables to allow access to */
/* free lists from inlined allocators without including gc_priv.h	 */
/* or introducing dependencies on internal data structure layouts.	 */
ptr_t * CONST GC_objfreelist_ptr = GC_objfreelist;
ptr_t * CONST GC_aobjfreelist_ptr = GC_aobjfreelist;
ptr_t * CONST GC_uobjfreelist_ptr = GC_uobjfreelist;
# ifdef ATOMIC_UNCOLLECTABLE
    ptr_t * CONST GC_auobjfreelist_ptr = GC_auobjfreelist;
# endif

/* Allocate a composite object of size n bytes.  The caller guarantees  */
/* that pointers past the first page are not relevant.  Caller holds    */
/* allocation lock.                                                     */
ptr_t GC_generic_malloc_inner_ignore_off_page(lb, k)
register size_t lb;
register int k;
{
    register struct hblk * h;
    register word n_blocks;
    register word lw;
    register ptr_t op;

    if (lb <= HBLKSIZE)
        return(GC_generic_malloc_inner((word)lb, k));
    n_blocks = divHBLKSZ(ADD_SLOP(lb) + HDR_BYTES + HBLKSIZE-1);
    if (!GC_is_initialized) GC_init_inner();
    /* Do our share of marking work */
    if(GC_incremental && !GC_dont_gc)
        GC_collect_a_little_inner((int)n_blocks);
    lw = ROUNDED_UP_WORDS(lb);
    h = GC_allochblk(lw, k, IGNORE_OFF_PAGE);
#   ifdef USE_MUNMAP
      if (0 == h) {
        GC_merge_unmapped();
        h = GC_allochblk(lw, k, IGNORE_OFF_PAGE);
      }
#   endif
    while (0 == h && GC_collect_or_expand(n_blocks, TRUE)) {
      h = GC_allochblk(lw, k, IGNORE_OFF_PAGE);
    }
    if (h == 0) {
        op = 0;
    } else {
        op = (ptr_t) (h -> hb_body);
        GC_words_wasted += BYTES_TO_WORDS(n_blocks * HBLKSIZE) - lw;
    }
    GC_words_allocd += lw;
    return((ptr_t)op);
}

ptr_t GC_generic_malloc_ignore_off_page(lb, k)
register size_t lb;
register int k;
{
    register ptr_t result;
    DCL_LOCK_STATE;
    
    GC_INVOKE_FINALIZERS();
    DISABLE_SIGNALS();
    LOCK();
    result = GC_generic_malloc_inner_ignore_off_page(lb,k);
    UNLOCK();
    ENABLE_SIGNALS();
    if (0 == result) {
        return((*GC_oom_fn)(lb));
    } else {
        return(result);
    }
}

# if defined(__STDC__) || defined(__cplusplus)
  void * GC_malloc_ignore_off_page(size_t lb)
# else
  char * GC_malloc_ignore_off_page(lb)
  register size_t lb;
# endif
{
    return((GC_PTR)GC_generic_malloc_ignore_off_page(lb, NORMAL));
}

# if defined(__STDC__) || defined(__cplusplus)
  void * GC_malloc_atomic_ignore_off_page(size_t lb)
# else
  char * GC_malloc_atomic_ignore_off_page(lb)
  register size_t lb;
# endif
{
    return((GC_PTR)GC_generic_malloc_ignore_off_page(lb, PTRFREE));
}

/* Increment GC_words_allocd from code that doesn't have direct access 	*/
/* to GC_arrays.							*/
# ifdef __STDC__
void GC_incr_words_allocd(size_t n)
{
    GC_words_allocd += n;
}

/* The same for GC_mem_freed.				*/
void GC_incr_mem_freed(size_t n)
{
    GC_mem_freed += n;
}
# endif /* __STDC__ */

/* Analogous to the above, but assumes a small object size, and 	*/
/* bypasses MERGE_SIZES mechanism.  Used by gc_inline.h.		*/
#ifdef __STDC__
     ptr_t GC_generic_malloc_words_small(size_t lw, int k)
#else 
     ptr_t GC_generic_malloc_words_small(lw, k)
     register word lw;
     register int k;
#endif
{
register ptr_t op;
register ptr_t *opp;
register struct obj_kind * kind = GC_obj_kinds + k;
DCL_LOCK_STATE;

    GC_INVOKE_FINALIZERS();
    DISABLE_SIGNALS();
    LOCK();
    opp = &(kind -> ok_freelist[lw]);
    if( (op = *opp) == 0 ) {
        if (!GC_is_initialized) {
            GC_init_inner();
        }
	if (kind -> ok_reclaim_list != 0 || GC_alloc_reclaim_list(kind)) {
	    op = GC_clear_stack(GC_allocobj((word)lw, k));
	}
	if (op == 0) {
	    UNLOCK();
	    ENABLE_SIGNALS();
	    return ((*GC_oom_fn)(WORDS_TO_BYTES(lw)));
	}
    }
    *opp = obj_link(op);
    obj_link(op) = 0;
    GC_words_allocd += lw;
    UNLOCK();
    ENABLE_SIGNALS();
    return((ptr_t)op);
}

#if defined(THREADS) && !defined(SRC_M3)
/* Return a list of 1 or more objects of the indicated size, linked	*/
/* through the first word in the object.  This has the advantage that	*/
/* it acquires the allocation lock only once, and may greatly reduce	*/
/* time wasted contending for the allocation lock.  Typical usage would */
/* be in a thread that requires many items of the same size.  It would	*/
/* keep its own free list in thread-local storage, and call		*/
/* GC_malloc_many or friends to replenish it.  (We do not round up	*/
/* object sizes, since a call indicates the intention to consume many	*/
/* objects of exactly this size.)					*/
/* Note that the client should usually clear the link field.		*/
ptr_t GC_generic_malloc_many(lb, k)
register word lb;
register int k;
{
ptr_t op;
register ptr_t p;
ptr_t *opp;
word lw;
register word my_words_allocd;
DCL_LOCK_STATE;

    if (!SMALL_OBJ(lb)) {
        op = GC_generic_malloc(lb, k);
        if(0 != op) obj_link(op) = 0;
        return(op);
    }
    lw = ALIGNED_WORDS(lb);
    GC_INVOKE_FINALIZERS();
    DISABLE_SIGNALS();
    LOCK();
    opp = &(GC_obj_kinds[k].ok_freelist[lw]);
    if( (op = *opp) == 0 ) {
        if (!GC_is_initialized) {
            GC_init_inner();
        }
	op = GC_clear_stack(GC_allocobj(lw, k));
	if (op == 0) {
	    UNLOCK();
	    ENABLE_SIGNALS();
	    op = (*GC_oom_fn)(lb);
	    if(0 != op) obj_link(op) = 0;
            return(op);
	}
    }
    *opp = 0;
    my_words_allocd = 0;
    for (p = op; p != 0; p = obj_link(p)) {
        my_words_allocd += lw;
        if (my_words_allocd >= BODY_SZ) {
            *opp = obj_link(p);
            obj_link(p) = 0;
            break;
        }
    }
    GC_words_allocd += my_words_allocd;
    
out:
    UNLOCK();
    ENABLE_SIGNALS();
    return(op);

}

void * GC_malloc_many(size_t lb)
{
    return(GC_generic_malloc_many(lb, NORMAL));
}

/* Note that the "atomic" version of this would be unsafe, since the	*/
/* links would not be seen by the collector.				*/
# endif

/* Allocate lb bytes of pointerful, traced, but not collectable data */
# ifdef __STDC__
    GC_PTR GC_malloc_uncollectable(size_t lb)
# else
    GC_PTR GC_malloc_uncollectable(lb)
    size_t lb;
# endif
{
register ptr_t op;
register ptr_t *opp;
register word lw;
DCL_LOCK_STATE;

    if( SMALL_OBJ(lb) ) {
#       ifdef MERGE_SIZES
#	  ifdef ADD_BYTE_AT_END
	    if (lb != 0) lb--;
	    	  /* We don't need the extra byte, since this won't be	*/
	    	  /* collected anyway.					*/
#	  endif
	  lw = GC_size_map[lb];
#	else
	  lw = ALIGNED_WORDS(lb);
#       endif
	opp = &(GC_uobjfreelist[lw]);
	FASTLOCK();
        if( FASTLOCK_SUCCEEDED() && (op = *opp) != 0 ) {
            /* See above comment on signals.	*/
            *opp = obj_link(op);
            obj_link(op) = 0;
            GC_words_allocd += lw;
            /* Mark bit ws already set on free list.  It will be	*/
	    /* cleared only temporarily during a collection, as a 	*/
	    /* result of the normal free list mark bit clearing.	*/
            GC_non_gc_bytes += WORDS_TO_BYTES(lw);
            FASTUNLOCK();
            return((GC_PTR) op);
        }
        FASTUNLOCK();
        op = (ptr_t)GC_generic_malloc((word)lb, UNCOLLECTABLE);
    } else {
	op = (ptr_t)GC_generic_malloc((word)lb, UNCOLLECTABLE);
    }
    if (0 == op) return(0);
    /* We don't need the lock here, since we have an undisguised 	*/
    /* pointer.  We do need to hold the lock while we adjust		*/
    /* mark bits.							*/
    {
	register struct hblk * h;
	
	h = HBLKPTR(op);
	lw = HDR(h) -> hb_sz;
	
	DISABLE_SIGNALS();
	LOCK();
	GC_set_mark_bit(op);
	GC_non_gc_bytes += WORDS_TO_BYTES(lw);
	UNLOCK();
	ENABLE_SIGNALS();
	return((GC_PTR) op);
    }
}

# ifdef ATOMIC_UNCOLLECTABLE
/* Allocate lb bytes of pointerfree, untraced, uncollectable data 	*/
/* This is normally roughly equivalent to the system malloc.		*/
/* But it may be useful if malloc is redefined.				*/
# ifdef __STDC__
    GC_PTR GC_malloc_atomic_uncollectable(size_t lb)
# else
    GC_PTR GC_malloc_atomic_uncollectable(lb)
    size_t lb;
# endif
{
register ptr_t op;
register ptr_t *opp;
register word lw;
DCL_LOCK_STATE;

    if( SMALL_OBJ(lb) ) {
#       ifdef MERGE_SIZES
#	  ifdef ADD_BYTE_AT_END
	    if (lb != 0) lb--;
	    	  /* We don't need the extra byte, since this won't be	*/
	    	  /* collected anyway.					*/
#	  endif
	  lw = GC_size_map[lb];
#	else
	  lw = ALIGNED_WORDS(lb);
#       endif
	opp = &(GC_auobjfreelist[lw]);
	FASTLOCK();
        if( FASTLOCK_SUCCEEDED() && (op = *opp) != 0 ) {
            /* See above comment on signals.	*/
            *opp = obj_link(op);
            obj_link(op) = 0;
            GC_words_allocd += lw;
	    /* Mark bit was already set while object was on free list. */
            GC_non_gc_bytes += WORDS_TO_BYTES(lw);
            FASTUNLOCK();
            return((GC_PTR) op);
        }
        FASTUNLOCK();
        op = (ptr_t)GC_generic_malloc((word)lb, AUNCOLLECTABLE);
    } else {
	op = (ptr_t)GC_generic_malloc((word)lb, AUNCOLLECTABLE);
    }
    if (0 == op) return(0);
    /* We don't need the lock here, since we have an undisguised 	*/
    /* pointer.  We do need to hold the lock while we adjust		*/
    /* mark bits.							*/
    {
	register struct hblk * h;
	
	h = HBLKPTR(op);
	lw = HDR(h) -> hb_sz;
	
	DISABLE_SIGNALS();
	LOCK();
	GC_set_mark_bit(op);
	GC_non_gc_bytes += WORDS_TO_BYTES(lw);
	UNLOCK();
	ENABLE_SIGNALS();
	return((GC_PTR) op);
    }
}

#endif /* ATOMIC_UNCOLLECTABLE */
