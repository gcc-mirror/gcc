
/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1995 by Xerox Corporation.  All rights reserved.
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


# include <stdio.h>
# include "gc_priv.h"
# include "gc_mark.h"

/* We put this here to minimize the risk of inlining. */
/*VARARGS*/
#ifdef __WATCOMC__
  void GC_noop(void *p, ...) {}
#else
  void GC_noop() {}
#endif

/* Single argument version, robust against whole program analysis. */
void GC_noop1(x)
word x;
{
    static VOLATILE word sink;

    sink = x;
}

/* mark_proc GC_mark_procs[MAX_MARK_PROCS] = {0} -- declared in gc_priv.h */

word GC_n_mark_procs = GC_RESERVED_MARK_PROCS;

/* Initialize GC_obj_kinds properly and standard free lists properly.  	*/
/* This must be done statically since they may be accessed before 	*/
/* GC_init is called.							*/
/* It's done here, since we need to deal with mark descriptors.		*/
struct obj_kind GC_obj_kinds[MAXOBJKINDS] = {
/* PTRFREE */ { &GC_aobjfreelist[0], 0 /* filled in dynamically */,
		0 | DS_LENGTH, FALSE, FALSE },
/* NORMAL  */ { &GC_objfreelist[0], 0,
#		if defined(ADD_BYTE_AT_END) && ALIGNMENT > DS_TAGS
		(word)(-ALIGNMENT) | DS_LENGTH,
#		else
		0 | DS_LENGTH,
#		endif
		TRUE /* add length to descr */, TRUE },
/* UNCOLLECTABLE */
	      { &GC_uobjfreelist[0], 0,
		0 | DS_LENGTH, TRUE /* add length to descr */, TRUE },
# ifdef ATOMIC_UNCOLLECTABLE
   /* AUNCOLLECTABLE */
	      { &GC_auobjfreelist[0], 0,
		0 | DS_LENGTH, FALSE /* add length to descr */, FALSE },
# endif
# ifdef STUBBORN_ALLOC
/*STUBBORN*/ { &GC_sobjfreelist[0], 0,
		0 | DS_LENGTH, TRUE /* add length to descr */, TRUE },
# endif
};

# ifdef ATOMIC_UNCOLLECTABLE
#   ifdef STUBBORN_ALLOC
      int GC_n_kinds = 5;
#   else
      int GC_n_kinds = 4;
#   endif
# else
#   ifdef STUBBORN_ALLOC
      int GC_n_kinds = 4;
#   else
      int GC_n_kinds = 3;
#   endif
# endif


# ifndef INITIAL_MARK_STACK_SIZE
#   define INITIAL_MARK_STACK_SIZE (1*HBLKSIZE)
		/* INITIAL_MARK_STACK_SIZE * sizeof(mse) should be a 	*/
		/* multiple of HBLKSIZE.				*/
		/* The incremental collector actually likes a larger	*/
		/* size, since it want to push all marked dirty objs	*/
		/* before marking anything new.  Currently we let it	*/
		/* grow dynamically.					*/
# endif

/*
 * Limits of stack for GC_mark routine.
 * All ranges between GC_mark_stack(incl.) and GC_mark_stack_top(incl.) still
 * need to be marked from.
 */

word GC_n_rescuing_pages;	/* Number of dirty pages we marked from */
				/* excludes ptrfree pages, etc.		*/

mse * GC_mark_stack;

word GC_mark_stack_size = 0;
 
mse * GC_mark_stack_top;

static struct hblk * scan_ptr;

mark_state_t GC_mark_state = MS_NONE;

GC_bool GC_mark_stack_too_small = FALSE;

GC_bool GC_objects_are_marked = FALSE;	/* Are there collectable marked	*/
					/* objects in the heap?		*/

/* Is a collection in progress?  Note that this can return true in the	*/
/* nonincremental case, if a collection has been abandoned and the	*/
/* mark state is now MS_INVALID.					*/
GC_bool GC_collection_in_progress()
{
    return(GC_mark_state != MS_NONE);
}

/* clear all mark bits in the header */
void GC_clear_hdr_marks(hhdr)
register hdr * hhdr;
{
    BZERO(hhdr -> hb_marks, MARK_BITS_SZ*sizeof(word));
}

/* Set all mark bits in the header.  Used for uncollectable blocks. */
void GC_set_hdr_marks(hhdr)
register hdr * hhdr;
{
    register int i;

    for (i = 0; i < MARK_BITS_SZ; ++i) {
    	hhdr -> hb_marks[i] = ONES;
    }
}

/*
 * Clear all mark bits associated with block h.
 */
/*ARGSUSED*/
static void clear_marks_for_block(h, dummy)
struct hblk *h;
word dummy;
{
    register hdr * hhdr = HDR(h);
    
    if (IS_UNCOLLECTABLE(hhdr -> hb_obj_kind)) return;
        /* Mark bit for these is cleared only once the object is 	*/
        /* explicitly deallocated.  This either frees the block, or	*/
        /* the bit is cleared once the object is on the free list.	*/
    GC_clear_hdr_marks(hhdr);
}

/* Slow but general routines for setting/clearing/asking about mark bits */
void GC_set_mark_bit(p)
ptr_t p;
{
    register struct hblk *h = HBLKPTR(p);
    register hdr * hhdr = HDR(h);
    register int word_no = (word *)p - (word *)h;
    
    set_mark_bit_from_hdr(hhdr, word_no);
}

void GC_clear_mark_bit(p)
ptr_t p;
{
    register struct hblk *h = HBLKPTR(p);
    register hdr * hhdr = HDR(h);
    register int word_no = (word *)p - (word *)h;
    
    clear_mark_bit_from_hdr(hhdr, word_no);
}

GC_bool GC_is_marked(p)
ptr_t p;
{
    register struct hblk *h = HBLKPTR(p);
    register hdr * hhdr = HDR(h);
    register int word_no = (word *)p - (word *)h;
    
    return(mark_bit_from_hdr(hhdr, word_no));
}


/*
 * Clear mark bits in all allocated heap blocks.  This invalidates
 * the marker invariant, and sets GC_mark_state to reflect this.
 * (This implicitly starts marking to reestablish the invariant.)
 */
void GC_clear_marks()
{
    GC_apply_to_all_blocks(clear_marks_for_block, (word)0);
    GC_objects_are_marked = FALSE;
    GC_mark_state = MS_INVALID;
    scan_ptr = 0;
#   ifdef GATHERSTATS
	/* Counters reflect currently marked objects: reset here */
        GC_composite_in_use = 0;
        GC_atomic_in_use = 0;
#   endif

}

/* Initiate a garbage collection.  Initiates a full collection if the	*/
/* mark	state is invalid.						*/
/*ARGSUSED*/
void GC_initiate_gc()
{
    if (GC_dirty_maintained) GC_read_dirty();
#   ifdef STUBBORN_ALLOC
    	GC_read_changed();
#   endif
#   ifdef CHECKSUMS
	{
	    extern void GC_check_dirty();
	    
	    if (GC_dirty_maintained) GC_check_dirty();
	}
#   endif
#   ifdef GATHERSTATS
	GC_n_rescuing_pages = 0;
#   endif
    if (GC_mark_state == MS_NONE) {
        GC_mark_state = MS_PUSH_RESCUERS;
    } else if (GC_mark_state != MS_INVALID) {
    	ABORT("unexpected state");
    } /* else this is really a full collection, and mark	*/
      /* bits are invalid.					*/
    scan_ptr = 0;
}


static void alloc_mark_stack();

/* Perform a small amount of marking.			*/
/* We try to touch roughly a page of memory.		*/
/* Return TRUE if we just finished a mark phase.	*/
/* Cold_gc_frame is an address inside a GC frame that	*/
/* remains valid until all marking is complete.		*/
/* A zero value indicates that it's OK to miss some	*/
/* register values.					*/
GC_bool GC_mark_some(cold_gc_frame)
ptr_t cold_gc_frame;
{
    switch(GC_mark_state) {
    	case MS_NONE:
    	    return(FALSE);
    	    
    	case MS_PUSH_RESCUERS:
    	    if (GC_mark_stack_top
    	        >= GC_mark_stack + GC_mark_stack_size
		   - INITIAL_MARK_STACK_SIZE/2) {
		/* Go ahead and mark, even though that might cause us to */
		/* see more marked dirty objects later on.  Avoid this	 */
		/* in the future.					 */
		GC_mark_stack_too_small = TRUE;
    	        GC_mark_from_mark_stack();
    	        return(FALSE);
    	    } else {
    	        scan_ptr = GC_push_next_marked_dirty(scan_ptr);
    	        if (scan_ptr == 0) {
#		    ifdef PRINTSTATS
			GC_printf1("Marked from %lu dirty pages\n",
				   (unsigned long)GC_n_rescuing_pages);
#		    endif
    	    	    GC_push_roots(FALSE, cold_gc_frame);
    	    	    GC_objects_are_marked = TRUE;
    	    	    if (GC_mark_state != MS_INVALID) {
    	    	        GC_mark_state = MS_ROOTS_PUSHED;
    	    	    }
    	    	}
    	    }
    	    return(FALSE);
    	
    	case MS_PUSH_UNCOLLECTABLE:
    	    if (GC_mark_stack_top
    	        >= GC_mark_stack + INITIAL_MARK_STACK_SIZE/4) {
    	        GC_mark_from_mark_stack();
    	        return(FALSE);
    	    } else {
    	        scan_ptr = GC_push_next_marked_uncollectable(scan_ptr);
    	        if (scan_ptr == 0) {
    	    	    GC_push_roots(TRUE, cold_gc_frame);
    	    	    GC_objects_are_marked = TRUE;
    	    	    if (GC_mark_state != MS_INVALID) {
    	    	        GC_mark_state = MS_ROOTS_PUSHED;
    	    	    }
    	    	}
    	    }
    	    return(FALSE);
    	
    	case MS_ROOTS_PUSHED:
    	    if (GC_mark_stack_top >= GC_mark_stack) {
    	        GC_mark_from_mark_stack();
    	        return(FALSE);
    	    } else {
    	        GC_mark_state = MS_NONE;
    	        if (GC_mark_stack_too_small) {
    	            alloc_mark_stack(2*GC_mark_stack_size);
    	        }
    	        return(TRUE);
    	    }
    	    
    	case MS_INVALID:
    	case MS_PARTIALLY_INVALID:
	    if (!GC_objects_are_marked) {
		GC_mark_state = MS_PUSH_UNCOLLECTABLE;
		return(FALSE);
	    }
    	    if (GC_mark_stack_top >= GC_mark_stack) {
    	        GC_mark_from_mark_stack();
    	        return(FALSE);
    	    }
    	    if (scan_ptr == 0 && GC_mark_state == MS_INVALID) {
		/* About to start a heap scan for marked objects. */
		/* Mark stack is empty.  OK to reallocate.	  */
		if (GC_mark_stack_too_small) {
    	            alloc_mark_stack(2*GC_mark_stack_size);
		}
		GC_mark_state = MS_PARTIALLY_INVALID;
    	    }
    	    scan_ptr = GC_push_next_marked(scan_ptr);
    	    if (scan_ptr == 0 && GC_mark_state == MS_PARTIALLY_INVALID) {
    	    	GC_push_roots(TRUE, cold_gc_frame);
    	    	GC_objects_are_marked = TRUE;
    	    	if (GC_mark_state != MS_INVALID) {
    	    	    GC_mark_state = MS_ROOTS_PUSHED;
    	    	}
    	    }
    	    return(FALSE);
    	default:
    	    ABORT("GC_mark_some: bad state");
    	    return(FALSE);
    }
}


GC_bool GC_mark_stack_empty()
{
    return(GC_mark_stack_top < GC_mark_stack);
}	

#ifdef PROF_MARKER
    word GC_prof_array[10];
#   define PROF(n) GC_prof_array[n]++
#else
#   define PROF(n)
#endif

/* Given a pointer to someplace other than a small object page or the	*/
/* first page of a large object, return a pointer either to the		*/
/* start of the large object or NIL.					*/
/* In the latter case black list the address current.			*/
/* Returns NIL without black listing if current points to a block	*/
/* with IGNORE_OFF_PAGE set.						*/
/*ARGSUSED*/
# ifdef PRINT_BLACK_LIST
  ptr_t GC_find_start(current, hhdr, source)
  word source;
# else
  ptr_t GC_find_start(current, hhdr)
# define source 0
# endif
register ptr_t current;
register hdr * hhdr;
{
#   ifdef ALL_INTERIOR_POINTERS
	if (hhdr != 0) {
	    register ptr_t orig = current;
	    
	    current = (ptr_t)HBLKPTR(current) + HDR_BYTES;
	    do {
	      current = current - HBLKSIZE*(word)hhdr;
	      hhdr = HDR(current);
	    } while(IS_FORWARDING_ADDR_OR_NIL(hhdr));
	    /* current points to the start of the large object */
	    if (hhdr -> hb_flags & IGNORE_OFF_PAGE) return(0);
	    if ((word *)orig - (word *)current
	         >= (ptrdiff_t)(hhdr->hb_sz)) {
	        /* Pointer past the end of the block */
	        GC_ADD_TO_BLACK_LIST_NORMAL(orig, source);
	        return(0);
	    }
	    return(current);
	} else {
	    GC_ADD_TO_BLACK_LIST_NORMAL(current, source);
	    return(0);
        }
#   else
        GC_ADD_TO_BLACK_LIST_NORMAL(current, source);
        return(0);
#   endif
#   undef source
}

void GC_invalidate_mark_state()
{
    GC_mark_state = MS_INVALID;
    GC_mark_stack_top = GC_mark_stack-1;
}

mse * GC_signal_mark_stack_overflow(msp)
mse * msp;
{
    GC_mark_state = MS_INVALID;
    GC_mark_stack_too_small = TRUE;
#   ifdef PRINTSTATS
	GC_printf1("Mark stack overflow; current size = %lu entries\n",
	    	    GC_mark_stack_size);
#    endif
     return(msp-INITIAL_MARK_STACK_SIZE/8);
}


/*
 * Mark objects pointed to by the regions described by
 * mark stack entries between GC_mark_stack and GC_mark_stack_top,
 * inclusive.  Assumes the upper limit of a mark stack entry
 * is never 0.  A mark stack entry never has size 0.
 * We try to traverse on the order of a hblk of memory before we return.
 * Caller is responsible for calling this until the mark stack is empty.
 * Note that this is the most performance critical routine in the
 * collector.  Hence it contains all sorts of ugly hacks to speed
 * things up.  In particular, we avoid procedure calls on the common
 * path, we take advantage of peculiarities of the mark descriptor
 * encoding, we optionally maintain a cache for the block address to
 * header mapping, we prefetch when an object is "grayed", etc. 
 */
void GC_mark_from_mark_stack()
{
  mse * GC_mark_stack_reg = GC_mark_stack;
  mse * GC_mark_stack_top_reg = GC_mark_stack_top;
  mse * mark_stack_limit = &(GC_mark_stack[GC_mark_stack_size]);
  int credit = HBLKSIZE;	/* Remaining credit for marking work	*/
  register word * current_p;	/* Pointer to current candidate ptr.	*/
  register word current;	/* Candidate pointer.			*/
  register word * limit;	/* (Incl) limit of current candidate 	*/
  				/* range				*/
  register word descr;
  register ptr_t greatest_ha = GC_greatest_plausible_heap_addr;
  register ptr_t least_ha = GC_least_plausible_heap_addr;
  DECLARE_HDR_CACHE;

# define SPLIT_RANGE_WORDS 128  /* Must be power of 2.		*/

  GC_objects_are_marked = TRUE;
  INIT_HDR_CACHE;
# ifdef OS2 /* Use untweaked version to circumvent compiler problem */
  while (GC_mark_stack_top_reg >= GC_mark_stack_reg && credit >= 0) {
# else
  while ((((ptr_t)GC_mark_stack_top_reg - (ptr_t)GC_mark_stack_reg) | credit)
  	>= 0) {
# endif
    current_p = GC_mark_stack_top_reg -> mse_start;
    descr = GC_mark_stack_top_reg -> mse_descr;
  retry:
    /* current_p and descr describe the current object.		*/
    /* *GC_mark_stack_top_reg is vacant.			*/
    /* The following is 0 only for small objects described by a simple	*/
    /* length descriptor.  For many applications this is the common	*/
    /* case, so we try to detect it quickly.				*/
    if (descr & ((~(WORDS_TO_BYTES(SPLIT_RANGE_WORDS) - 1)) | DS_TAGS)) {
      word tag = descr & DS_TAGS;
      
      switch(tag) {
        case DS_LENGTH:
          /* Large length.					        */
          /* Process part of the range to avoid pushing too much on the	*/
          /* stack.							*/
          GC_mark_stack_top_reg -> mse_start =
         	limit = current_p + SPLIT_RANGE_WORDS-1;
          GC_mark_stack_top_reg -> mse_descr =
          		descr - WORDS_TO_BYTES(SPLIT_RANGE_WORDS-1);
          /* Make sure that pointers overlapping the two ranges are	*/
          /* considered. 						*/
          limit = (word *)((char *)limit + sizeof(word) - ALIGNMENT);
          break;
        case DS_BITMAP:
          GC_mark_stack_top_reg--;
          descr &= ~DS_TAGS;
          credit -= WORDS_TO_BYTES(WORDSZ/2); /* guess */
          while (descr != 0) {
            if ((signed_word)descr < 0) {
              current = *current_p;
	      if ((ptr_t)current >= least_ha && (ptr_t)current < greatest_ha) {
                PUSH_CONTENTS((ptr_t)current, GC_mark_stack_top_reg,
			      mark_stack_limit, current_p, exit1);
	      }
            }
	    descr <<= 1;
	    ++ current_p;
          }
          continue;
        case DS_PROC:
          GC_mark_stack_top_reg--;
          credit -= PROC_BYTES;
#ifdef GC_DEBUG
	  current_p = GC_debug_object_start(current_p);
#endif
          GC_mark_stack_top_reg =
              (*PROC(descr))
              	    (current_p, GC_mark_stack_top_reg,
              	    mark_stack_limit, ENV(descr));
          continue;
        case DS_PER_OBJECT:
	  if ((signed_word)descr >= 0) {
	    /* Descriptor is in the object.	*/
            descr = *(word *)((ptr_t)current_p + descr - DS_PER_OBJECT);
	  } else {
	    /* Descriptor is in type descriptor pointed to by first	*/
	    /* word in object.						*/
	    ptr_t type_descr = *(ptr_t *)current_p;
	    /* type_descr is either a valid pointer to the descriptor	*/
	    /* structure, or this object was on a free list.  If it 	*/
	    /* it was anything but the last object on the free list,	*/
	    /* we will misinterpret the next object on the free list as */
	    /* the type descriptor, and get a 0 GC descriptor, which	*/
	    /* is ideal.  Unfortunately, we need to check for the last	*/
	    /* object case explicitly.					*/
	    if (0 == type_descr) {
		/* Rarely executed.	*/
		GC_mark_stack_top_reg--;
		continue;
	    }
            descr = *(word *)(type_descr
			      - (descr - (DS_PER_OBJECT - INDIR_PER_OBJ_BIAS)));
	  }
          goto retry;
      }
    } else /* Small object with length descriptor */ {
      GC_mark_stack_top_reg--;
      limit = (word *)(((ptr_t)current_p) + (word)descr);
    }
    /* The simple case in which we're scanning a range.	*/
    credit -= (ptr_t)limit - (ptr_t)current_p;
    limit -= 1;
    {
#     define PREF_DIST 4

#     ifndef SMALL_CONFIG
        word deferred;

	/* Try to prefetch the next pointer to be examined asap.	*/
	/* Empirically, this also seems to help slightly without	*/
	/* prefetches, at least on linux/X86.  Presumably this loop 	*/
	/* ends up with less register pressure, and gcc thus ends up 	*/
	/* generating slightly better code.  Overall gcc code quality	*/
	/* for this loop is still not great.				*/
	for(;;) {
	  PREFETCH((ptr_t)limit - PREF_DIST*CACHE_LINE_SIZE);
	  deferred = *limit;
	  limit = (word *)((char *)limit - ALIGNMENT);
	  if ((ptr_t)deferred >= least_ha && (ptr_t)deferred <  greatest_ha) {
	    PREFETCH(deferred);
	    break;
	  }
	  if (current_p > limit) goto next_object;
	  /* Unroll once, so we don't do too many of the prefetches 	*/
	  /* based on limit.						*/
	  deferred = *limit;
	  limit = (word *)((char *)limit - ALIGNMENT);
	  if ((ptr_t)deferred >= least_ha && (ptr_t)deferred <  greatest_ha) {
	    PREFETCH(deferred);
	    break;
	  }
	  if (current_p > limit) goto next_object;
	}
#     endif

      while (current_p <= limit) {
	/* Empirically, unrolling this loop doesn't help a lot.	*/
	/* Since HC_PUSH_CONTENTS expands to a lot of code,	*/
	/* we don't.						*/
        current = *current_p;
        PREFETCH((ptr_t)current_p + PREF_DIST*CACHE_LINE_SIZE);
        if ((ptr_t)current >= least_ha && (ptr_t)current <  greatest_ha) {
  	  /* Prefetch the contents of the object we just pushed.  It's	*/
  	  /* likely we will need them soon.				*/
  	  PREFETCH(current);
          HC_PUSH_CONTENTS((ptr_t)current, GC_mark_stack_top_reg,
  		           mark_stack_limit, current_p, exit2);
        }
        current_p = (word *)((char *)current_p + ALIGNMENT);
      }

#     ifndef SMALL_CONFIG
	/* We still need to mark the entry we previously prefetched.	*/
	/* We alrady know that it passes the preliminary pointer	*/
	/* validity test.						*/
        HC_PUSH_CONTENTS((ptr_t)deferred, GC_mark_stack_top_reg,
  		         mark_stack_limit, current_p, exit4);
	next_object:;
#     endif
    }
  }
  GC_mark_stack_top = GC_mark_stack_top_reg;
}

/* Allocate or reallocate space for mark stack of size s words  */
/* May silently fail.						*/
static void alloc_mark_stack(n)
word n;
{
    mse * new_stack = (mse *)GC_scratch_alloc(n * sizeof(struct ms_entry));
    
    GC_mark_stack_too_small = FALSE;
    if (GC_mark_stack_size != 0) {
        if (new_stack != 0) {
          word displ = (word)GC_mark_stack & (GC_page_size - 1);
          signed_word size = GC_mark_stack_size * sizeof(struct ms_entry);
          
          /* Recycle old space */
	      if (0 != displ) displ = GC_page_size - displ;
	      size = (size - displ) & ~(GC_page_size - 1);
	      if (size > 0) {
	        GC_add_to_heap((struct hblk *)
	      			((word)GC_mark_stack + displ), (word)size);
	      }
          GC_mark_stack = new_stack;
          GC_mark_stack_size = n;
#	  ifdef PRINTSTATS
	      GC_printf1("Grew mark stack to %lu frames\n",
		    	 (unsigned long) GC_mark_stack_size);
#	  endif
        } else {
#	  ifdef PRINTSTATS
	      GC_printf1("Failed to grow mark stack to %lu frames\n",
		    	 (unsigned long) n);
#	  endif
        }
    } else {
        if (new_stack == 0) {
            GC_err_printf0("No space for mark stack\n");
            EXIT();
        }
        GC_mark_stack = new_stack;
        GC_mark_stack_size = n;
    }
    GC_mark_stack_top = GC_mark_stack-1;
}

void GC_mark_init()
{
    alloc_mark_stack(INITIAL_MARK_STACK_SIZE);
}

/*
 * Push all locations between b and t onto the mark stack.
 * b is the first location to be checked. t is one past the last
 * location to be checked.
 * Should only be used if there is no possibility of mark stack
 * overflow.
 */
void GC_push_all(bottom, top)
ptr_t bottom;
ptr_t top;
{
    register word length;
    
    bottom = (ptr_t)(((word) bottom + ALIGNMENT-1) & ~(ALIGNMENT-1));
    top = (ptr_t)(((word) top) & ~(ALIGNMENT-1));
    if (top == 0 || bottom == top) return;
    GC_mark_stack_top++;
    if (GC_mark_stack_top >= GC_mark_stack + GC_mark_stack_size) {
	ABORT("unexpected mark stack overflow");
    }
    length = top - bottom;
#   if DS_TAGS > ALIGNMENT - 1
	length += DS_TAGS;
	length &= ~DS_TAGS;
#   endif
    GC_mark_stack_top -> mse_start = (word *)bottom;
    GC_mark_stack_top -> mse_descr = length;
}

/*
 * Analogous to the above, but push only those pages that may have been
 * dirtied.  A block h is assumed dirty if dirty_fn(h) != 0.
 * We use push_fn to actually push the block.
 * Will not overflow mark stack if push_fn pushes a small fixed number
 * of entries.  (This is invoked only if push_fn pushes a single entry,
 * or if it marks each object before pushing it, thus ensuring progress
 * in the event of a stack overflow.)
 */
void GC_push_dirty(bottom, top, dirty_fn, push_fn)
ptr_t bottom;
ptr_t top;
int (*dirty_fn)(/* struct hblk * h */);
void (*push_fn)(/* ptr_t bottom, ptr_t top */);
{
    register struct hblk * h;

    bottom = (ptr_t)(((long) bottom + ALIGNMENT-1) & ~(ALIGNMENT-1));
    top = (ptr_t)(((long) top) & ~(ALIGNMENT-1));

    if (top == 0 || bottom == top) return;
    h = HBLKPTR(bottom + HBLKSIZE);
    if (top <= (ptr_t) h) {
  	if ((*dirty_fn)(h-1)) {
	    (*push_fn)(bottom, top);
	}
	return;
    }
    if ((*dirty_fn)(h-1)) {
        (*push_fn)(bottom, (ptr_t)h);
    }
    while ((ptr_t)(h+1) <= top) {
	if ((*dirty_fn)(h)) {
	    if ((word)(GC_mark_stack_top - GC_mark_stack)
		> 3 * GC_mark_stack_size / 4) {
	 	/* Danger of mark stack overflow */
		(*push_fn)((ptr_t)h, top);
		return;
	    } else {
		(*push_fn)((ptr_t)h, (ptr_t)(h+1));
	    }
	}
	h++;
    }
    if ((ptr_t)h != top) {
	if ((*dirty_fn)(h)) {
            (*push_fn)((ptr_t)h, top);
        }
    }
    if (GC_mark_stack_top >= GC_mark_stack + GC_mark_stack_size) {
        ABORT("unexpected mark stack overflow");
    }
}

# ifndef SMALL_CONFIG
void GC_push_conditional(bottom, top, all)
ptr_t bottom;
ptr_t top;
int all;
{
    if (all) {
      if (GC_dirty_maintained) {
#	ifdef PROC_VDB
	    /* Pages that were never dirtied cannot contain pointers	*/
	    GC_push_dirty(bottom, top, GC_page_was_ever_dirty, GC_push_all);
#	else
	    GC_push_all(bottom, top);
#	endif
      } else {
      	GC_push_all(bottom, top);
      }
    } else {
	GC_push_dirty(bottom, top, GC_page_was_dirty, GC_push_all);
    }
}
#endif

# ifdef MSWIN32
  void __cdecl GC_push_one(p)
# else
  void GC_push_one(p)
# endif
word p;
{
#   ifdef NURSERY
      if (0 != GC_push_proc) {
	GC_push_proc(p);
	return;
      }
#   endif
    GC_PUSH_ONE_STACK(p, MARKED_FROM_REGISTER);
}

# ifdef __STDC__
#   define BASE(p) (word)GC_base((void *)(p))
# else
#   define BASE(p) (word)GC_base((char *)(p))
# endif

/* As above, but argument passed preliminary test. */
# if defined(PRINT_BLACK_LIST) || defined(KEEP_BACK_PTRS)
    void GC_push_one_checked(p, interior_ptrs, source)
    ptr_t source;
# else
    void GC_push_one_checked(p, interior_ptrs)
#   define source 0
# endif
register word p;
register GC_bool interior_ptrs;
{
    register word r;
    register hdr * hhdr; 
    register int displ;
  
    GET_HDR(p, hhdr);
    if (IS_FORWARDING_ADDR_OR_NIL(hhdr)) {
        if (hhdr != 0 && interior_ptrs) {
          r = BASE(p);
	  hhdr = HDR(r);
	  displ = BYTES_TO_WORDS(HBLKDISPL(r));
	} else {
	  hhdr = 0;
	}
    } else {
        register map_entry_type map_entry;
        
        displ = HBLKDISPL(p);
        map_entry = MAP_ENTRY((hhdr -> hb_map), displ);
        if (map_entry == OBJ_INVALID) {
#	  ifndef ALL_INTERIOR_POINTERS
            if (interior_ptrs) {
              r = BASE(p);
	      displ = BYTES_TO_WORDS(HBLKDISPL(r));
	      if (r == 0) hhdr = 0;
            } else {
              hhdr = 0;
            }
#	  else
	    /* map already reflects interior pointers */
	    hhdr = 0;
#	  endif
        } else {
          displ = BYTES_TO_WORDS(displ);
          displ -= map_entry;
          r = (word)((word *)(HBLKPTR(p)) + displ);
        }
    }
    /* If hhdr != 0 then r == GC_base(p), only we did it faster. */
    /* displ is the word index within the block.		 */
    if (hhdr == 0) {
    	if (interior_ptrs) {
#	    ifdef PRINT_BLACK_LIST
	      GC_add_to_black_list_stack(p, source);
#	    else
	      GC_add_to_black_list_stack(p);
#	    endif
	} else {
	    GC_ADD_TO_BLACK_LIST_NORMAL(p, source);
#	    undef source  /* In case we had to define it. */
	}
    } else {
	if (!mark_bit_from_hdr(hhdr, displ)) {
	    set_mark_bit_from_hdr(hhdr, displ);
 	    GC_STORE_BACK_PTR(source, (ptr_t)r);
	    PUSH_OBJ((word *)r, hhdr, GC_mark_stack_top,
	             &(GC_mark_stack[GC_mark_stack_size]));
	}
    }
}

# ifdef TRACE_BUF

# define TRACE_ENTRIES 1000

struct trace_entry {
    char * kind;
    word gc_no;
    word words_allocd;
    word arg1;
    word arg2;
} GC_trace_buf[TRACE_ENTRIES];

int GC_trace_buf_ptr = 0;

void GC_add_trace_entry(char *kind, word arg1, word arg2)
{
    GC_trace_buf[GC_trace_buf_ptr].kind = kind;
    GC_trace_buf[GC_trace_buf_ptr].gc_no = GC_gc_no;
    GC_trace_buf[GC_trace_buf_ptr].words_allocd = GC_words_allocd;
    GC_trace_buf[GC_trace_buf_ptr].arg1 = arg1 ^ 0x80000000;
    GC_trace_buf[GC_trace_buf_ptr].arg2 = arg2 ^ 0x80000000;
    GC_trace_buf_ptr++;
    if (GC_trace_buf_ptr >= TRACE_ENTRIES) GC_trace_buf_ptr = 0;
}

void GC_print_trace(word gc_no, GC_bool lock)
{
    int i;
    struct trace_entry *p;
    
    if (lock) LOCK();
    for (i = GC_trace_buf_ptr-1; i != GC_trace_buf_ptr; i--) {
    	if (i < 0) i = TRACE_ENTRIES-1;
    	p = GC_trace_buf + i;
    	if (p -> gc_no < gc_no || p -> kind == 0) return;
    	printf("Trace:%s (gc:%d,words:%d) 0x%X, 0x%X\n",
    		p -> kind, p -> gc_no, p -> words_allocd,
    		(p -> arg1) ^ 0x80000000, (p -> arg2) ^ 0x80000000);
    }
    printf("Trace incomplete\n");
    if (lock) UNLOCK();
}

# endif /* TRACE_BUF */

/*
 * A version of GC_push_all that treats all interior pointers as valid
 * and scans the entire region immediately, in case the contents
 * change.
 */
void GC_push_all_eager(bottom, top)
ptr_t bottom;
ptr_t top;
{
    word * b = (word *)(((long) bottom + ALIGNMENT-1) & ~(ALIGNMENT-1));
    word * t = (word *)(((long) top) & ~(ALIGNMENT-1));
    register word *p;
    register word q;
    register word *lim;
    register ptr_t greatest_ha = GC_greatest_plausible_heap_addr;
    register ptr_t least_ha = GC_least_plausible_heap_addr;
#   define GC_greatest_plausible_heap_addr greatest_ha
#   define GC_least_plausible_heap_addr least_ha

    if (top == 0) return;
    /* check all pointers in range and put in push if they appear */
    /* to be valid.						  */
      lim = t - 1 /* longword */;
      for (p = b; p <= lim; p = (word *)(((char *)p) + ALIGNMENT)) {
	q = *p;
	GC_PUSH_ONE_STACK(q, p);
      }
#   undef GC_greatest_plausible_heap_addr
#   undef GC_least_plausible_heap_addr
}

#ifndef THREADS
/*
 * A version of GC_push_all that treats all interior pointers as valid
 * and scans part of the area immediately, to make sure that saved
 * register values are not lost.
 * Cold_gc_frame delimits the stack section that must be scanned
 * eagerly.  A zero value indicates that no eager scanning is needed.
 */
void GC_push_all_stack_partially_eager(bottom, top, cold_gc_frame)
ptr_t bottom;
ptr_t top;
ptr_t cold_gc_frame;
{
# ifdef ALL_INTERIOR_POINTERS
#   define EAGER_BYTES 1024
    /* Push the hot end of the stack eagerly, so that register values   */
    /* saved inside GC frames are marked before they disappear.		*/
    /* The rest of the marking can be deferred until later.		*/
    if (0 == cold_gc_frame) {
	GC_push_all_stack(bottom, top);
	return;
    }
#   ifdef STACK_GROWS_DOWN
	GC_push_all_eager(bottom, cold_gc_frame);
	GC_push_all(cold_gc_frame - sizeof(ptr_t), top);
#   else /* STACK_GROWS_UP */
	GC_push_all_eager(cold_gc_frame, top);
	GC_push_all(bottom, cold_gc_frame + sizeof(ptr_t));
#   endif /* STACK_GROWS_UP */
# else
    GC_push_all_eager(bottom, top);
# endif
# ifdef TRACE_BUF
      GC_add_trace_entry("GC_push_all_stack", bottom, top);
# endif
}
#endif /* !THREADS */

void GC_push_all_stack(bottom, top)
ptr_t bottom;
ptr_t top;
{
# ifdef ALL_INTERIOR_POINTERS
    GC_push_all(bottom, top);
# else
    GC_push_all_eager(bottom, top);
# endif
}

#ifndef SMALL_CONFIG
/* Push all objects reachable from marked objects in the given block */
/* of size 1 objects.						     */
void GC_push_marked1(h, hhdr)
struct hblk *h;
register hdr * hhdr;
{
    word * mark_word_addr = &(hhdr->hb_marks[divWORDSZ(HDR_WORDS)]);
    register word *p;
    word *plim;
    register int i;
    register word q;
    register word mark_word;
    register ptr_t greatest_ha = GC_greatest_plausible_heap_addr;
    register ptr_t least_ha = GC_least_plausible_heap_addr;
#   define GC_greatest_plausible_heap_addr greatest_ha
#   define GC_least_plausible_heap_addr least_ha
    
    p = (word *)(h->hb_body);
    plim = (word *)(((word)h) + HBLKSIZE);

    /* go through all words in block */
	while( p < plim )  {
	    mark_word = *mark_word_addr++;
	    i = 0;
	    while(mark_word != 0) {
	      if (mark_word & 1) {
	          q = p[i];
	          GC_PUSH_ONE_HEAP(q, p + i);
	      }
	      i++;
	      mark_word >>= 1;
	    }
	    p += WORDSZ;
	}
#   undef GC_greatest_plausible_heap_addr
#   undef GC_least_plausible_heap_addr        
}


#ifndef UNALIGNED

/* Push all objects reachable from marked objects in the given block */
/* of size 2 objects.						     */
void GC_push_marked2(h, hhdr)
struct hblk *h;
register hdr * hhdr;
{
    word * mark_word_addr = &(hhdr->hb_marks[divWORDSZ(HDR_WORDS)]);
    register word *p;
    word *plim;
    register int i;
    register word q;
    register word mark_word;
    register ptr_t greatest_ha = GC_greatest_plausible_heap_addr;
    register ptr_t least_ha = GC_least_plausible_heap_addr;
#   define GC_greatest_plausible_heap_addr greatest_ha
#   define GC_least_plausible_heap_addr least_ha
    
    p = (word *)(h->hb_body);
    plim = (word *)(((word)h) + HBLKSIZE);

    /* go through all words in block */
	while( p < plim )  {
	    mark_word = *mark_word_addr++;
	    i = 0;
	    while(mark_word != 0) {
	      if (mark_word & 1) {
	          q = p[i];
	          GC_PUSH_ONE_HEAP(q, p + i);
	          q = p[i+1];
	          GC_PUSH_ONE_HEAP(q, p + i);
	      }
	      i += 2;
	      mark_word >>= 2;
	    }
	    p += WORDSZ;
	}
#   undef GC_greatest_plausible_heap_addr
#   undef GC_least_plausible_heap_addr        
}

/* Push all objects reachable from marked objects in the given block */
/* of size 4 objects.						     */
/* There is a risk of mark stack overflow here.  But we handle that. */
/* And only unmarked objects get pushed, so it's not very likely.    */
void GC_push_marked4(h, hhdr)
struct hblk *h;
register hdr * hhdr;
{
    word * mark_word_addr = &(hhdr->hb_marks[divWORDSZ(HDR_WORDS)]);
    register word *p;
    word *plim;
    register int i;
    register word q;
    register word mark_word;
    register ptr_t greatest_ha = GC_greatest_plausible_heap_addr;
    register ptr_t least_ha = GC_least_plausible_heap_addr;
#   define GC_greatest_plausible_heap_addr greatest_ha
#   define GC_least_plausible_heap_addr least_ha
    
    p = (word *)(h->hb_body);
    plim = (word *)(((word)h) + HBLKSIZE);

    /* go through all words in block */
	while( p < plim )  {
	    mark_word = *mark_word_addr++;
	    i = 0;
	    while(mark_word != 0) {
	      if (mark_word & 1) {
	          q = p[i];
	          GC_PUSH_ONE_HEAP(q, p + i);
	          q = p[i+1];
	          GC_PUSH_ONE_HEAP(q, p + i + 1);
	          q = p[i+2];
	          GC_PUSH_ONE_HEAP(q, p + i + 2);
	          q = p[i+3];
	          GC_PUSH_ONE_HEAP(q, p + i + 3);
	      }
	      i += 4;
	      mark_word >>= 4;
	    }
	    p += WORDSZ;
	}
#   undef GC_greatest_plausible_heap_addr
#   undef GC_least_plausible_heap_addr        
}

#endif /* UNALIGNED */

#endif /* SMALL_CONFIG */

/* Push all objects reachable from marked objects in the given block */
void GC_push_marked(h, hhdr)
struct hblk *h;
register hdr * hhdr;
{
    register int sz = hhdr -> hb_sz;
    register word * p;
    register int word_no;
    register word * lim;
    register mse * GC_mark_stack_top_reg;
    register mse * mark_stack_limit = &(GC_mark_stack[GC_mark_stack_size]);
    
    /* Some quick shortcuts: */
	{ 
	    struct obj_kind *ok = &(GC_obj_kinds[hhdr -> hb_obj_kind]);
	    if ((0 | DS_LENGTH) == ok -> ok_descriptor
		&& FALSE == ok -> ok_relocate_descr)
		return;
	}
        if (GC_block_empty(hhdr)/* nothing marked */) return;
#   ifdef GATHERSTATS
        GC_n_rescuing_pages++;
#   endif
    GC_objects_are_marked = TRUE;
    if (sz > MAXOBJSZ) {
        lim = (word *)(h + 1);
    } else {
        lim = (word *)(h + 1) - sz;
    }
    
    switch(sz) {
#   if !defined(SMALL_CONFIG)    
     case 1:
       GC_push_marked1(h, hhdr);
       break;
#   endif
#   if !defined(SMALL_CONFIG) && !defined(UNALIGNED)
     case 2:
       GC_push_marked2(h, hhdr);
       break;
     case 4:
       GC_push_marked4(h, hhdr);
       break;
#   endif       
     default:
      GC_mark_stack_top_reg = GC_mark_stack_top;
      for (p = (word *)h + HDR_WORDS, word_no = HDR_WORDS; p <= lim;
         p += sz, word_no += sz) {
         /* This ignores user specified mark procs.  This currently	*/
         /* doesn't matter, since marking from the whole object		*/
         /* is always sufficient, and we will eventually use the user	*/
         /* mark proc to avoid any bogus pointers.			*/
         if (mark_bit_from_hdr(hhdr, word_no)) {
           /* Mark from fields inside the object */
             PUSH_OBJ((word *)p, hhdr, GC_mark_stack_top_reg, mark_stack_limit);
#	     ifdef GATHERSTATS
		/* Subtract this object from total, since it was	*/
		/* added in twice.					*/
		GC_composite_in_use -= sz;
#	     endif
         }
      }
      GC_mark_stack_top = GC_mark_stack_top_reg;
    }
}

#ifndef SMALL_CONFIG
/* Test whether any page in the given block is dirty	*/
GC_bool GC_block_was_dirty(h, hhdr)
struct hblk *h;
register hdr * hhdr;
{
    register int sz = hhdr -> hb_sz;
    
    if (sz < MAXOBJSZ) {
         return(GC_page_was_dirty(h));
    } else {
    	 register ptr_t p = (ptr_t)h;
         sz += HDR_WORDS;
         sz = WORDS_TO_BYTES(sz);
         while (p < (ptr_t)h + sz) {
             if (GC_page_was_dirty((struct hblk *)p)) return(TRUE);
             p += HBLKSIZE;
         }
         return(FALSE);
    }
}
#endif /* SMALL_CONFIG */

/* Similar to GC_push_next_marked, but return address of next block	*/
struct hblk * GC_push_next_marked(h)
struct hblk *h;
{
    register hdr * hhdr;
    
    h = GC_next_used_block(h);
    if (h == 0) return(0);
    hhdr = HDR(h);
    GC_push_marked(h, hhdr);
    return(h + OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz));
}

#ifndef SMALL_CONFIG
/* Identical to above, but mark only from dirty pages	*/
struct hblk * GC_push_next_marked_dirty(h)
struct hblk *h;
{
    register hdr * hhdr;
    
    if (!GC_dirty_maintained) { ABORT("dirty bits not set up"); }
    for (;;) {
        h = GC_next_used_block(h);
        if (h == 0) return(0);
        hhdr = HDR(h);
#	ifdef STUBBORN_ALLOC
          if (hhdr -> hb_obj_kind == STUBBORN) {
            if (GC_page_was_changed(h) && GC_block_was_dirty(h, hhdr)) {
                break;
            }
          } else {
            if (GC_block_was_dirty(h, hhdr)) break;
          }
#	else
	  if (GC_block_was_dirty(h, hhdr)) break;
#	endif
        h += OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz);
    }
    GC_push_marked(h, hhdr);
    return(h + OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz));
}
#endif

/* Similar to above, but for uncollectable pages.  Needed since we	*/
/* do not clear marks for such pages, even for full collections.	*/
struct hblk * GC_push_next_marked_uncollectable(h)
struct hblk *h;
{
    register hdr * hhdr = HDR(h);
    
    for (;;) {
        h = GC_next_used_block(h);
        if (h == 0) return(0);
        hhdr = HDR(h);
	if (hhdr -> hb_obj_kind == UNCOLLECTABLE) break;
        h += OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz);
    }
    GC_push_marked(h, hhdr);
    return(h + OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz));
}


