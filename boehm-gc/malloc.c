/* 
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
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
/* Boehm, February 7, 1996 4:32 pm PST */
 
#include <stdio.h>
#include "gc_priv.h"

extern ptr_t GC_clear_stack();	/* in misc.c, behaves like identity */
void GC_extend_size_map();	/* in misc.c. */

/* Allocate reclaim list for kind:	*/
/* Return TRUE on success		*/
GC_bool GC_alloc_reclaim_list(kind)
register struct obj_kind * kind;
{
    struct hblk ** result = (struct hblk **)
    		GC_scratch_alloc((MAXOBJSZ+1) * sizeof(struct hblk *));
    if (result == 0) return(FALSE);
    BZERO(result, (MAXOBJSZ+1)*sizeof(struct hblk *));
    kind -> ok_reclaim_list = result;
    return(TRUE);
}

/* allocate lb bytes for an object of kind.	*/
/* Should not be used to directly to allocate	*/
/* objects such as STUBBORN objects that	*/
/* require special handling on allocation.	*/
/* First a version that assumes we already	*/
/* hold lock:					*/
ptr_t GC_generic_malloc_inner(lb, k)
register word lb;
register int k;
{
register word lw;
register ptr_t op;
register ptr_t *opp;

    if( SMALL_OBJ(lb) ) {
        register struct obj_kind * kind = GC_obj_kinds + k;
#       ifdef MERGE_SIZES
	  lw = GC_size_map[lb];
#	else
	  lw = ALIGNED_WORDS(lb);
	  if (lw == 0) lw = 1;
#       endif
	opp = &(kind -> ok_freelist[lw]);
        if( (op = *opp) == 0 ) {
#	    ifdef MERGE_SIZES
	      if (GC_size_map[lb] == 0) {
	        if (!GC_is_initialized)  GC_init_inner();
	        if (GC_size_map[lb] == 0) GC_extend_size_map(lb);
	        return(GC_generic_malloc_inner(lb, k));
	      }
#	    else
	      if (!GC_is_initialized) {
	        GC_init_inner();
	        return(GC_generic_malloc_inner(lb, k));
	      }
#	    endif
	    if (kind -> ok_reclaim_list == 0) {
	    	if (!GC_alloc_reclaim_list(kind)) goto out;
	    }
	    op = GC_allocobj(lw, k);
	    if (op == 0) goto out;
        }
        /* Here everything is in a consistent state.	*/
        /* We assume the following assignment is	*/
        /* atomic.  If we get aborted			*/
        /* after the assignment, we lose an object,	*/
        /* but that's benign.				*/
        /* Volatile declarations may need to be added	*/
        /* to prevent the compiler from breaking things.*/
        *opp = obj_link(op);
        obj_link(op) = 0;
    } else {
	register struct hblk * h;
	register word n_blocks = divHBLKSZ(ADD_SLOP(lb)
					   + HDR_BYTES + HBLKSIZE-1);
	
	if (!GC_is_initialized) GC_init_inner();
	/* Do our share of marking work */
          if(GC_incremental && !GC_dont_gc)
		GC_collect_a_little_inner((int)n_blocks);
	lw = ROUNDED_UP_WORDS(lb);
        h = GC_allochblk(lw, k, 0);
#       ifdef USE_MUNMAP
	  if (0 == h) {
	    GC_merge_unmapped();
	    h = GC_allochblk(lw, k, 0);
	  }
#	endif
	while (0 == h && GC_collect_or_expand(n_blocks, FALSE)) {
	  h = GC_allochblk(lw, k, 0);
	}
	if (h == 0) {
	    op = 0;
	} else {
	    op = (ptr_t) (h -> hb_body);
	    GC_words_wasted += BYTES_TO_WORDS(n_blocks * HBLKSIZE) - lw;
	}
    }
    GC_words_allocd += lw;
    
out:
    return((ptr_t)op);
}

ptr_t GC_generic_malloc(lb, k)
register word lb;
register int k;
{
    ptr_t result;
    DCL_LOCK_STATE;

    GC_INVOKE_FINALIZERS();
    DISABLE_SIGNALS();
    LOCK();
    result = GC_generic_malloc_inner(lb, k);
    UNLOCK();
    ENABLE_SIGNALS();
    if (0 == result) {
        return((*GC_oom_fn)(lb));
    } else {
        return(result);
    }
}   


#define GENERAL_MALLOC(lb,k) \
    (GC_PTR)GC_clear_stack(GC_generic_malloc((word)lb, k))
/* We make the GC_clear_stack_call a tail call, hoping to get more of	*/
/* the stack.								*/

/* Allocate lb bytes of atomic (pointerfree) data */
# ifdef __STDC__
    GC_PTR GC_malloc_atomic(size_t lb)
# else
    GC_PTR GC_malloc_atomic(lb)
    size_t lb;
# endif
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
	opp = &(GC_aobjfreelist[lw]);
	FASTLOCK();
        if( !FASTLOCK_SUCCEEDED() || (op = *opp) == 0 ) {
            FASTUNLOCK();
            return(GENERAL_MALLOC((word)lb, PTRFREE));
        }
        /* See above comment on signals.	*/
        *opp = obj_link(op);
        GC_words_allocd += lw;
        FASTUNLOCK();
        return((GC_PTR) op);
   } else {
       return(GENERAL_MALLOC((word)lb, PTRFREE));
   }
}

/* Allocate lb bytes of composite (pointerful) data */
# ifdef __STDC__
    GC_PTR GC_malloc(size_t lb)
# else
    GC_PTR GC_malloc(lb)
    size_t lb;
# endif
{
register ptr_t op;
register ptr_t *opp;
register word lw;
DCL_LOCK_STATE;

    if( SMALL_OBJ(lb) ) {
#       ifdef MERGE_SIZES
	  lw = GC_size_map[lb];
#	else
	  lw = ALIGNED_WORDS(lb);
#       endif
	opp = &(GC_objfreelist[lw]);
	FASTLOCK();
        if( !FASTLOCK_SUCCEEDED() || (op = *opp) == 0 ) {
            FASTUNLOCK();
            return(GENERAL_MALLOC((word)lb, NORMAL));
        }
        /* See above comment on signals.	*/
        *opp = obj_link(op);
        obj_link(op) = 0;
        GC_words_allocd += lw;
        FASTUNLOCK();
        return((GC_PTR) op);
   } else {
       return(GENERAL_MALLOC((word)lb, NORMAL));
   }
}

# ifdef REDIRECT_MALLOC
# ifdef __STDC__
    GC_PTR malloc(size_t lb)
# else
    GC_PTR malloc(lb)
    size_t lb;
# endif
  {
    /* It might help to manually inline the GC_malloc call here.	*/
    /* But any decent compiler should reduce the extra procedure call	*/
    /* to at most a jump instruction in this case.			*/
#   if defined(I386) && defined(SOLARIS_THREADS)
      /*
       * Thread initialisation can call malloc before
       * we're ready for it.
       * It's not clear that this is enough to help matters.
       * The thread implementation may well call malloc at other
       * inopportune times.
       */
      if (!GC_is_initialized) return sbrk(lb);
#   endif /* I386 && SOLARIS_THREADS */
    return(REDIRECT_MALLOC(lb));
  }

# ifdef __STDC__
    GC_PTR calloc(size_t n, size_t lb)
# else
    GC_PTR calloc(n, lb)
    size_t n, lb;
# endif
  {
    return(REDIRECT_MALLOC(n*lb));
  }
# endif /* REDIRECT_MALLOC */

GC_PTR GC_generic_or_special_malloc(lb,knd)
word lb;
int knd;
{
    switch(knd) {
#     ifdef STUBBORN_ALLOC
	case STUBBORN:
	    return(GC_malloc_stubborn((size_t)lb));
#     endif
	case PTRFREE:
	    return(GC_malloc_atomic((size_t)lb));
	case NORMAL:
	    return(GC_malloc((size_t)lb));
	case UNCOLLECTABLE:
	    return(GC_malloc_uncollectable((size_t)lb));
#       ifdef ATOMIC_UNCOLLECTABLE
	  case AUNCOLLECTABLE:
	    return(GC_malloc_atomic_uncollectable((size_t)lb));
#	endif /* ATOMIC_UNCOLLECTABLE */
	default:
	    return(GC_generic_malloc(lb,knd));
    }
}


/* Change the size of the block pointed to by p to contain at least   */
/* lb bytes.  The object may be (and quite likely will be) moved.     */
/* The kind (e.g. atomic) is the same as that of the old.	      */
/* Shrinking of large blocks is not implemented well.                 */
# ifdef __STDC__
    GC_PTR GC_realloc(GC_PTR p, size_t lb)
# else
    GC_PTR GC_realloc(p,lb)
    GC_PTR p;
    size_t lb;
# endif
{
register struct hblk * h;
register hdr * hhdr;
register word sz;	 /* Current size in bytes	*/
register word orig_sz;	 /* Original sz in bytes	*/
int obj_kind;

    if (p == 0) return(GC_malloc(lb));	/* Required by ANSI */
    h = HBLKPTR(p);
    hhdr = HDR(h);
    sz = hhdr -> hb_sz;
    obj_kind = hhdr -> hb_obj_kind;
    sz = WORDS_TO_BYTES(sz);
    orig_sz = sz;

    if (sz > WORDS_TO_BYTES(MAXOBJSZ)) {
	/* Round it up to the next whole heap block */
	  register word descr;
	  
	  sz = (sz+HDR_BYTES+HBLKSIZE-1)
		& (~HBLKMASK);
	  sz -= HDR_BYTES;
	  hhdr -> hb_sz = BYTES_TO_WORDS(sz);
	  descr = GC_obj_kinds[obj_kind].ok_descriptor;
          if (GC_obj_kinds[obj_kind].ok_relocate_descr) descr += sz;
          hhdr -> hb_descr = descr;
	  if (IS_UNCOLLECTABLE(obj_kind)) GC_non_gc_bytes += (sz - orig_sz);
	  /* Extra area is already cleared by allochblk. */
    }
    if (ADD_SLOP(lb) <= sz) {
	if (lb >= (sz >> 1)) {
#	    ifdef STUBBORN_ALLOC
	        if (obj_kind == STUBBORN) GC_change_stubborn(p);
#	    endif
	    if (orig_sz > lb) {
	      /* Clear unneeded part of object to avoid bogus pointer */
	      /* tracing.					      */
	      /* Safe for stubborn objects.			      */
	        BZERO(((ptr_t)p) + lb, orig_sz - lb);
	    }
	    return(p);
	} else {
	    /* shrink */
	      GC_PTR result =
	      		GC_generic_or_special_malloc((word)lb, obj_kind);

	      if (result == 0) return(0);
	          /* Could also return original object.  But this 	*/
	          /* gives the client warning of imminent disaster.	*/
	      BCOPY(p, result, lb);
#	      ifndef IGNORE_FREE
	        GC_free(p);
#	      endif
	      return(result);
	}
    } else {
	/* grow */
	  GC_PTR result =
	  	GC_generic_or_special_malloc((word)lb, obj_kind);

	  if (result == 0) return(0);
	  BCOPY(p, result, sz);
#	  ifndef IGNORE_FREE
	    GC_free(p);
#	  endif
	  return(result);
    }
}

# ifdef REDIRECT_MALLOC
# ifdef __STDC__
    GC_PTR realloc(GC_PTR p, size_t lb)
# else
    GC_PTR realloc(p,lb)
    GC_PTR p;
    size_t lb;
# endif
  {
    return(GC_realloc(p, lb));
  }
# endif /* REDIRECT_MALLOC */

/* Explicitly deallocate an object p.				*/
# ifdef __STDC__
    void GC_free(GC_PTR p)
# else
    void GC_free(p)
    GC_PTR p;
# endif
{
    register struct hblk *h;
    register hdr *hhdr;
    register signed_word sz;
    register ptr_t * flh;
    register int knd;
    register struct obj_kind * ok;
    DCL_LOCK_STATE;

    if (p == 0) return;
    	/* Required by ANSI.  It's not my fault ...	*/
    h = HBLKPTR(p);
    hhdr = HDR(h);
#   if defined(REDIRECT_MALLOC) && \
	(defined(SOLARIS_THREADS) || defined(LINUX_THREADS))
	/* We have to redirect malloc calls during initialization.	*/
	/* Don't try to deallocate that memory.				*/
	if (0 == hhdr) return;
#   endif
    knd = hhdr -> hb_obj_kind;
    sz = hhdr -> hb_sz;
    ok = &GC_obj_kinds[knd];
    if (sz <= MAXOBJSZ) {
#	ifdef THREADS
	    DISABLE_SIGNALS();
	    LOCK();
#	endif
	GC_mem_freed += sz;
	/* A signal here can make GC_mem_freed and GC_non_gc_bytes	*/
	/* inconsistent.  We claim this is benign.			*/
	if (IS_UNCOLLECTABLE(knd)) GC_non_gc_bytes -= WORDS_TO_BYTES(sz);
		/* Its unnecessary to clear the mark bit.  If the 	*/
		/* object is reallocated, it doesn't matter.  O.w. the	*/
		/* collector will do it, since it's on a free list.	*/
	if (ok -> ok_init) {
	    BZERO((word *)p + 1, WORDS_TO_BYTES(sz-1));
	}
	flh = &(ok -> ok_freelist[sz]);
	obj_link(p) = *flh;
	*flh = (ptr_t)p;
#	ifdef THREADS
	    UNLOCK();
	    ENABLE_SIGNALS();
#	endif
    } else {
    	DISABLE_SIGNALS();
        LOCK();
        GC_mem_freed += sz;
	if (IS_UNCOLLECTABLE(knd)) GC_non_gc_bytes -= WORDS_TO_BYTES(sz);
        GC_freehblk(h);
        UNLOCK();
        ENABLE_SIGNALS();
    }
}

# ifdef REDIRECT_MALLOC
#   ifdef __STDC__
      void free(GC_PTR p)
#   else
      void free(p)
      GC_PTR p;
#   endif
  {
#   ifndef IGNORE_FREE
      GC_free(p);
#   endif
  }
# endif  /* REDIRECT_MALLOC */
