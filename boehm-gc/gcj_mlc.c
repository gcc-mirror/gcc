/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1999 by Hewlett-Packard Company.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
 */
/* Boehm, July 31, 1995 5:02 pm PDT */

#ifdef GC_GCJ_SUPPORT

/*
 * This is an allocator interface tuned for gcj (the GNU/Cygnus static
 * java compiler).
 *
 * Each allocated object has a pointer in its first word to a vtable,
 * which for our purposes is simply a structure describing the type of
 * the object.
 * This descriptor structur contains a GC marking descriptor at offset
 * MARK_DESCR_OFFSET.
 *
 * It is hoped that this interface may also be useful for other systems,
 * possibly with some tuning of the constants.  But the immediate goal
 * is to get better gcj performance.
 *
 * We assume:
 *  1) We have an ANSI conforming C compiler.
 *  2) Counting on explicit initialization of this interface is OK.
 *  3) FASTLOCK is not a significant win.
 */

#include "gc_priv.h"
#include "gc_mark.h"
#include "include/gc_gcj.h"
#include "dbg_mlc.h"

GC_bool GC_gcj_malloc_initialized = FALSE;

int GC_gcj_kind;	/* Object kind for objects with descriptors     */
			/* in "vtable".					*/
int GC_gcj_debug_kind;	/* The kind of objects that is always marked 	*/
			/* with a mark proc call.			*/

ptr_t * GC_gcjobjfreelist;
ptr_t * GC_gcjdebugobjfreelist;

void * GC_default_oom_action(void) { return 0; }

void * (*GC_oom_action)(void) = GC_default_oom_action;

/* Caller does not hold allocation lock. */
void GC_init_gcj_malloc(int mp_index, void * /* really mark_proc */mp)
{
    register int i;
    DCL_LOCK_STATE;

    GC_init();	/* In case it's not already done.	*/
    DISABLE_SIGNALS();
    LOCK();
    if (GC_gcj_malloc_initialized) {
      UNLOCK();
      ENABLE_SIGNALS();
      return;
    }
    GC_gcj_malloc_initialized = TRUE;
    GC_mark_procs[mp_index] = (mark_proc)mp;
    if (mp_index >= GC_n_mark_procs) ABORT("GC_init_gcj_malloc: bad index");
    /* Set up object kind gcj-style indirect descriptor. */
      GC_gcjobjfreelist = (ptr_t *)
          GC_generic_malloc_inner((MAXOBJSZ+1)*sizeof(ptr_t), PTRFREE);
      if (GC_gcjobjfreelist == 0) ABORT("Couldn't allocate GC_gcjobjfreelist");
      BZERO(GC_gcjobjfreelist, (MAXOBJSZ+1)*sizeof(ptr_t));
      GC_gcj_kind = GC_n_kinds++;
      GC_obj_kinds[GC_gcj_kind].ok_freelist = GC_gcjobjfreelist;
      GC_obj_kinds[GC_gcj_kind].ok_reclaim_list = 0;
      GC_obj_kinds[GC_gcj_kind].ok_descriptor =
    	(((word)(-MARK_DESCR_OFFSET - INDIR_PER_OBJ_BIAS)) | DS_PER_OBJECT);
      GC_obj_kinds[GC_gcj_kind].ok_relocate_descr = FALSE;
      GC_obj_kinds[GC_gcj_kind].ok_init = TRUE;
    /* Set up object kind for objects that require mark proc call.	*/
      GC_gcjdebugobjfreelist = (ptr_t *)
          GC_generic_malloc_inner((MAXOBJSZ+1)*sizeof(ptr_t), PTRFREE);
      if (GC_gcjdebugobjfreelist == 0)
	  ABORT("Couldn't allocate GC_gcjdebugobjfreelist");
      BZERO(GC_gcjdebugobjfreelist, (MAXOBJSZ+1)*sizeof(ptr_t));
      GC_gcj_debug_kind = GC_n_kinds++;
      GC_obj_kinds[GC_gcj_debug_kind].ok_freelist = GC_gcjdebugobjfreelist;
      GC_obj_kinds[GC_gcj_debug_kind].ok_reclaim_list = 0;
      GC_obj_kinds[GC_gcj_debug_kind].ok_descriptor =
    	MAKE_PROC(mp_index, 1 /* allocated with debug info */);
      GC_obj_kinds[GC_gcj_debug_kind].ok_relocate_descr = FALSE;
      GC_obj_kinds[GC_gcj_debug_kind].ok_init = TRUE;
    UNLOCK();
    ENABLE_SIGNALS();
}

ptr_t GC_clear_stack();

#define GENERAL_MALLOC(lb,k) \
    (GC_PTR)GC_clear_stack(GC_generic_malloc_inner((word)lb, k))
    
#define GENERAL_MALLOC_IOP(lb,k) \
    (GC_PTR)GC_clear_stack(GC_generic_malloc_inner_ignore_off_page(lb, k))

/* Allocate an object, clear it, and store the pointer to the	*/
/* type structure (vtable in gcj).				*/
/* This adds a byte at the end of the object if GC_malloc would.*/
void * GC_gcj_malloc(size_t lb, void * ptr_to_struct_containing_descr)
{
register ptr_t op;
register ptr_t * opp;
register word lw;
DCL_LOCK_STATE;

    if( SMALL_OBJ(lb) ) {
#       ifdef MERGE_SIZES
	  lw = GC_size_map[lb];
#	else
	  lw = ALIGNED_WORDS(lb);
#       endif
	opp = &(GC_gcjobjfreelist[lw]);
	LOCK();
        if( (op = *opp) == 0 ) {
            op = (ptr_t)GENERAL_MALLOC((word)lb, GC_gcj_kind);
	    if (0 == op) {
		UNLOCK();
		return(GC_oom_action());
	    }
#	    ifdef MERGE_SIZES
		lw = GC_size_map[lb];	/* May have been uninitialized.	*/
#	    endif
        } else {
            *opp = obj_link(op);
            GC_words_allocd += lw;
            FASTUNLOCK();
        }
	*(void **)op = ptr_to_struct_containing_descr;
	UNLOCK();
    } else {
	LOCK();
	op = (ptr_t)GENERAL_MALLOC((word)lb, GC_gcj_kind);
	if (0 == op) {
	    UNLOCK();
	    return(GC_oom_action());
	}
	*(void **)op = ptr_to_struct_containing_descr;
	UNLOCK();
    }
    return((GC_PTR) op);
}

/* Similar to GC_gcj_malloc, but add debug info.  This is allocated	*/
/* with GC_gcj_debug_kind.						*/
GC_PTR GC_debug_gcj_malloc(size_t lb, void * ptr_to_struct_containing_descr,
			   GC_EXTRA_PARAMS)
{
    GC_PTR result;

    /* We clone the code from GC_debug_gcj_malloc, so that we 	*/
    /* dont end up with extra frames on the stack, which could	*/
    /* confuse the backtrace.					*/
    LOCK();
    result = GC_generic_malloc_inner(lb + DEBUG_BYTES, GC_gcj_debug_kind);
    if (result == 0) {
	UNLOCK();
        GC_err_printf2("GC_debug_gcj_malloc(%ld, 0x%lx) returning NIL (",
        	       (unsigned long) lb,
		       (unsigned long) ptr_to_struct_containing_descr);
        GC_err_puts(s);
        GC_err_printf1(":%ld)\n", (unsigned long)i);
        return(GC_oom_action());
    }
    *((void **)((ptr_t)result + sizeof(oh))) = ptr_to_struct_containing_descr;
    UNLOCK();
    if (!GC_debugging_started) {
    	GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (GC_store_debug_info(result, (word)lb, s, (word)i));
}

/* Similar to GC_gcj_malloc, but the size is in words, and we don't	*/
/* adjust it.  The size is assumed to be such that it can be 	*/
/* allocated as a small object.					*/
void * GC_gcj_fast_malloc(size_t lw, void * ptr_to_struct_containing_descr)
{
ptr_t op;
ptr_t * opp;
DCL_LOCK_STATE;

    opp = &(GC_gcjobjfreelist[lw]);
    LOCK();
    if( (op = *opp) == 0 ) {
        op = (ptr_t)GC_clear_stack(
		GC_generic_malloc_words_small_inner(lw, GC_gcj_kind));
	if (0 == op) {
	    UNLOCK();
	    return(GC_oom_action());
	}
    } else {
        *opp = obj_link(op);
        GC_words_allocd += lw;
    }
    *(void **)op = ptr_to_struct_containing_descr;
    UNLOCK();
    return((GC_PTR) op);
}

/* And a debugging version of the above:	*/
void * GC_debug_gcj_fast_malloc(size_t lw,
				void * ptr_to_struct_containing_descr,
				GC_EXTRA_PARAMS)
{
    GC_PTR result;
    size_t lb = WORDS_TO_BYTES(lw);

    /* We clone the code from GC_debug_gcj_malloc, so that we 	*/
    /* dont end up with extra frames on the stack, which could	*/
    /* confuse the backtrace.					*/
    LOCK();
    result = GC_generic_malloc_inner(lb + DEBUG_BYTES, GC_gcj_debug_kind);
    if (result == 0) {
	UNLOCK();
        GC_err_printf2("GC_debug_gcj_fast_malloc(%ld, 0x%lx) returning NIL (",
        	       (unsigned long) lw,
		       (unsigned long) ptr_to_struct_containing_descr);
        GC_err_puts(s);
        GC_err_printf1(":%ld)\n", (unsigned long)i);
        return(GC_oom_action());
    }
    *((void **)((ptr_t)result + sizeof(oh))) = ptr_to_struct_containing_descr;
    UNLOCK();
    if (!GC_debugging_started) {
    	GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (GC_store_debug_info(result, (word)lb, s, (word)i));
}

void * GC_gcj_malloc_ignore_off_page(size_t lb,
				     void * ptr_to_struct_containing_descr) 
{
register ptr_t op;
register ptr_t * opp;
register word lw;
DCL_LOCK_STATE;

    if( SMALL_OBJ(lb) ) {
#       ifdef MERGE_SIZES
	  lw = GC_size_map[lb];
#	else
	  lw = ALIGNED_WORDS(lb);
#       endif
	opp = &(GC_gcjobjfreelist[lw]);
	LOCK();
        if( (op = *opp) == 0 ) {
            op = (ptr_t)GENERAL_MALLOC_IOP(lb, GC_gcj_kind);
#	    ifdef MERGE_SIZES
		lw = GC_size_map[lb];	/* May have been uninitialized.	*/
#	    endif
        } else {
            *opp = obj_link(op);
            GC_words_allocd += lw;
            FASTUNLOCK();
        }
	*(void **)op = ptr_to_struct_containing_descr;
	UNLOCK();
    } else {
        op = (ptr_t)GENERAL_MALLOC_IOP(lb, GC_gcj_kind);
        if (0 != op) {
          *(void **)op = ptr_to_struct_containing_descr;
	}
        UNLOCK();
    }
    return((GC_PTR) op);
}

#endif  /* GC_GCJ_SUPPORT */
