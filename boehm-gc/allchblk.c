/* 
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1998 by Silicon Graphics.  All rights reserved.
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
/* Boehm, August 9, 1995 5:08 pm PDT */

#define DEBUG
#undef DEBUG
#include <stdio.h>
#include "gc_priv.h"


/*
 * allocate/free routines for heap blocks
 * Note that everything called from outside the garbage collector
 * should be prepared to abort at any point as the result of a signal.
 */

/*
 * Free heap blocks are kept on a list sorted by address.
 * The hb_hdr.hbh_sz field of a free heap block contains the length
 * (in bytes) of the entire block.
 * Neighbors are coalesced.
 */
 
# define MAX_BLACK_LIST_ALLOC (2*HBLKSIZE)
		/* largest block we will allocate starting on a black   */
		/* listed block.  Must be >= HBLKSIZE.			*/

struct hblk * GC_hblkfreelist = 0;

struct hblk *GC_savhbp = (struct hblk *)0;  /* heap block preceding next */
					 /* block to be examined by   */
					 /* GC_allochblk.                */

# if !defined(NO_DEBUGGING)
void GC_print_hblkfreelist()
{
    struct hblk * h = GC_hblkfreelist;
    word total_free = 0;
    hdr * hhdr = HDR(h);
    word sz;
    
    while (h != 0) {
        sz = hhdr -> hb_sz;
    	GC_printf2("0x%lx size %lu ", (unsigned long)h, (unsigned long)sz);
    	total_free += sz;
        if (GC_is_black_listed(h, HBLKSIZE) != 0) {
             GC_printf0("start black listed\n");
        } else if (GC_is_black_listed(h, hhdr -> hb_sz) != 0) {
             GC_printf0("partially black listed\n");
        } else {
             GC_printf0("not black listed\n");
        }
        h = hhdr -> hb_next;
        hhdr = HDR(h);
    }
    GC_printf1("Total of %lu bytes on free list\n", (unsigned long)total_free);
}

# endif /* NO_DEBUGGING */

/* Initialize hdr for a block containing the indicated size and 	*/
/* kind of objects.							*/
/* Return FALSE on failure.						*/
static GC_bool setup_header(hhdr, sz, kind, flags)
register hdr * hhdr;
word sz;	/* object size in words */
int kind;
unsigned char flags;
{
    register word descr;
    
    /* Add description of valid object pointers */
      if (!GC_add_map_entry(sz)) return(FALSE);
      hhdr -> hb_map = GC_obj_map[sz > MAXOBJSZ? 0 : sz];
      
    /* Set size, kind and mark proc fields */
      hhdr -> hb_sz = sz;
      hhdr -> hb_obj_kind = kind;
      hhdr -> hb_flags = flags;
      descr = GC_obj_kinds[kind].ok_descriptor;
      if (GC_obj_kinds[kind].ok_relocate_descr) descr += WORDS_TO_BYTES(sz);
      hhdr -> hb_descr = descr;
      
    /* Clear mark bits */
      GC_clear_hdr_marks(hhdr);
      
    hhdr -> hb_last_reclaimed = (unsigned short)GC_gc_no;
    return(TRUE);
}

#ifdef EXACT_FIRST
#   define LAST_TRIP 2
#else
#   define LAST_TRIP 1
#endif
	
/*
 * Allocate (and return pointer to) a heap block
 *   for objects of size sz words.
 *
 * NOTE: We set obj_map field in header correctly.
 *       Caller is resposnsible for building an object freelist in block.
 *
 * We clear the block if it is destined for large objects, and if
 * kind requires that newly allocated objects be cleared.
 */
struct hblk *
GC_allochblk(sz, kind, flags)
word sz;
int kind;
unsigned char flags;  /* IGNORE_OFF_PAGE or 0 */
{
    register struct hblk *thishbp;
    register hdr * thishdr;		/* Header corr. to thishbp */
    register struct hblk *hbp;
    register hdr * hhdr;		/* Header corr. to hbp */
    struct hblk *prevhbp;
    register hdr * phdr;		/* Header corr. to prevhbp */
    signed_word size_needed;    /* number of bytes in requested objects */
    signed_word size_avail;	/* bytes available in this block	*/
    int trip_count = 0;

    size_needed = HBLKSIZE * OBJ_SZ_TO_BLOCKS(sz);

    /* search for a big enough block in free list */
	hbp = GC_savhbp;
	hhdr = HDR(hbp);
	for(;;) {

	    prevhbp = hbp;
	    phdr = hhdr;
	    hbp = (prevhbp == 0? GC_hblkfreelist : phdr->hb_next);
	    hhdr = HDR(hbp);

	    if( prevhbp == GC_savhbp) {
		if (trip_count == LAST_TRIP) return(0);
		++trip_count;
	    }

	    if( hbp == 0 ) continue;

	    size_avail = hhdr->hb_sz;
#	    ifdef EXACT_FIRST
		if (trip_count <= 1 && size_avail != size_needed) continue;
#	    endif
	    if (size_avail < size_needed) continue;
#	    ifdef PRESERVE_LAST
		if (size_avail != size_needed
		    && !GC_incremental
		    && GC_in_last_heap_sect(hbp) && GC_should_collect()) {
		    continue;
		} 
#	    endif
	    /* If the next heap block is obviously better, go on.	*/
	    /* This prevents us from disassembling a single large block */
	    /* to get tiny blocks.					*/
	    {
	      signed_word next_size;
	      
	      thishbp = hhdr -> hb_next;
	      if (thishbp == 0) thishbp = GC_hblkfreelist; 
	      thishdr = HDR(thishbp);
	      next_size = (signed_word)(thishdr -> hb_sz);
	      if (next_size < size_avail
	          && next_size >= size_needed
	          && !GC_is_black_listed(thishbp, (word)size_needed)) {
	          continue;
	      }
	    }
	    if ( !IS_UNCOLLECTABLE(kind) &&
	         (kind != PTRFREE || size_needed > MAX_BLACK_LIST_ALLOC)) {
	      struct hblk * lasthbp = hbp;
	      ptr_t search_end = (ptr_t)hbp + size_avail - size_needed;
	      signed_word orig_avail = size_avail;
	      signed_word eff_size_needed = ((flags & IGNORE_OFF_PAGE)?
	      					HBLKSIZE
	      					: size_needed);
	      
	      
	      while ((ptr_t)lasthbp <= search_end
	             && (thishbp = GC_is_black_listed(lasthbp,
	             				      (word)eff_size_needed))) {
	        lasthbp = thishbp;
	      }
	      size_avail -= (ptr_t)lasthbp - (ptr_t)hbp;
	      thishbp = lasthbp;
	      if (size_avail >= size_needed) {
	        if (thishbp != hbp && GC_install_header(thishbp)) {
	          /* Split the block at thishbp */
	              thishdr = HDR(thishbp);
	              /* GC_invalidate_map not needed, since we will	*/
	              /* allocate this block.				*/
		      thishdr -> hb_next = hhdr -> hb_next;
		      thishdr -> hb_sz = size_avail;
		      hhdr -> hb_sz = (ptr_t)thishbp - (ptr_t)hbp;
		      hhdr -> hb_next = thishbp;
		  /* Advance to thishbp */
		      prevhbp = hbp;
		      phdr = hhdr;
		      hbp = thishbp;
		      hhdr = thishdr;
		}
	      } else if (size_needed > (signed_word)BL_LIMIT
	                 && orig_avail - size_needed
			    > (signed_word)BL_LIMIT) {
	        /* Punt, since anything else risks unreasonable heap growth. */
	        WARN("Needed to allocate blacklisted block at 0x%lx\n",
		     (word)hbp);
	        thishbp = hbp;
	        size_avail = orig_avail;
	      } else if (size_avail == 0
	      		 && size_needed == HBLKSIZE
	      		 && prevhbp != 0) {
#		ifndef FIND_LEAK
	      	  static unsigned count = 0;
	      	  
	      	  /* The block is completely blacklisted.  We need 	*/
	      	  /* to drop some such blocks, since otherwise we spend */
	      	  /* all our time traversing them if pointerfree	*/
	      	  /* blocks are unpopular.				*/
	          /* A dropped block will be reconsidered at next GC.	*/
	          if ((++count & 3) == 0) {
	            /* Allocate and drop the block in small chunks, to	*/
	            /* maximize the chance that we will recover some	*/
	            /* later.						*/
	              struct hblk * limit = hbp + (hhdr->hb_sz/HBLKSIZE);
	              struct hblk * h;
	              
		      GC_words_wasted += hhdr->hb_sz;
	              phdr -> hb_next = hhdr -> hb_next;
	              for (h = hbp; h < limit; h++) {
	                if (h == hbp || GC_install_header(h)) {
	                  hhdr = HDR(h);
	                  (void) setup_header(
	                	  hhdr,
	              		  BYTES_TO_WORDS(HBLKSIZE - HDR_BYTES),
	              		  PTRFREE, 0); /* Cant fail */
	              	  if (GC_debugging_started) {
	              	    BZERO(hbp + HDR_BYTES, HBLKSIZE - HDR_BYTES);
	              	  }
	                }
	              }
	            /* Restore hbp to point at free block */
	              if (GC_savhbp == hbp) GC_savhbp = prevhbp;
	              hbp = prevhbp;
	              hhdr = phdr;
	              if (hbp == GC_savhbp) --trip_count;
	          }
#		endif
	      }
	    }
	    if( size_avail >= size_needed ) {
		/* found a big enough block       */
		/* let thishbp --> the block      */
		/* set prevhbp, hbp to bracket it */
		    thishbp = hbp;
		    thishdr = hhdr;
		    if( size_avail == size_needed ) {
			hbp = hhdr->hb_next;
			hhdr = HDR(hbp);
		    } else {
			hbp = (struct hblk *)
			    (((word)thishbp) + size_needed);
			if (!GC_install_header(hbp)) {
			    hbp = thishbp;
			    continue;
			}
			hhdr = HDR(hbp);
			GC_invalidate_map(hhdr);
			hhdr->hb_next = thishdr->hb_next;
			hhdr->hb_sz = size_avail - size_needed;
		    }
		/* remove *thishbp from hblk freelist */
		    if( prevhbp == 0 ) {
			GC_hblkfreelist = hbp;
		    } else {
			phdr->hb_next = hbp;
		    }
		/* save current list search position */
		    GC_savhbp = hbp;
		break;
	    }
	}
	
    /* Notify virtual dirty bit implementation that we are about to write. */
    	GC_write_hint(thishbp);
    
    /* Add it to map of valid blocks */
    	if (!GC_install_counts(thishbp, (word)size_needed)) return(0);
    	/* This leaks memory under very rare conditions. */
    		
    /* Set up header */
        if (!setup_header(thishdr, sz, kind, flags)) {
            GC_remove_counts(thishbp, (word)size_needed);
            return(0); /* ditto */
        }
        
    /* Clear block if necessary */
	if (GC_debugging_started
	    || sz > MAXOBJSZ && GC_obj_kinds[kind].ok_init) {
	    BZERO(thishbp + HDR_BYTES,  size_needed - HDR_BYTES);
	}

    /* We just successfully allocated a block.  Restart count of	*/
    /* consecutive failures.						*/
    {
	extern unsigned GC_fail_count;
	
	GC_fail_count = 0;
    }
    
    return( thishbp );
}
 
struct hblk * GC_freehblk_ptr = 0;  /* Search position hint for GC_freehblk */

/*
 * Free a heap block.
 *
 * Coalesce the block with its neighbors if possible.
 *
 * All mark words are assumed to be cleared.
 */
void
GC_freehblk(p)
register struct hblk *p;
{
register hdr *phdr;	/* Header corresponding to p */
register struct hblk *hbp, *prevhbp;
register hdr *hhdr, *prevhdr;
register signed_word size;

    /* GC_savhbp may become invalid due to coalescing.  Clear it. */
	GC_savhbp = (struct hblk *)0;

    phdr = HDR(p);
    size = phdr->hb_sz;
    size = HBLKSIZE * OBJ_SZ_TO_BLOCKS(size);
    GC_remove_counts(p, (word)size);
    phdr->hb_sz = size;
    GC_invalidate_map(phdr);
    prevhbp = 0;
    
    /* The following optimization was suggested by David Detlefs.	*/
    /* Note that the header cannot be NIL, since there cannot be an	*/
    /* intervening  call to GC_freehblk without resetting		*/
    /* GC_freehblk_ptr.							*/
    if (GC_freehblk_ptr != 0 &&
    	HDR(GC_freehblk_ptr)->hb_map == GC_invalid_map &&
    	(ptr_t)GC_freehblk_ptr < (ptr_t)p) {
      hbp = GC_freehblk_ptr;
    } else {
      hbp = GC_hblkfreelist;
    };
    hhdr = HDR(hbp);

    while( (hbp != 0) && (hbp < p) ) {
	prevhbp = hbp;
	prevhdr = hhdr;
	hbp = hhdr->hb_next;
	hhdr = HDR(hbp);
    }
    GC_freehblk_ptr = prevhbp;
    
    /* Check for duplicate deallocation in the easy case */
      if (hbp != 0 && (ptr_t)p + size > (ptr_t)hbp
        || prevhbp != 0 && (ptr_t)prevhbp + prevhdr->hb_sz > (ptr_t)p) {
        GC_printf1("Duplicate large block deallocation of 0x%lx\n",
        	   (unsigned long) p);
        GC_printf2("Surrounding free blocks are 0x%lx and 0x%lx\n",
           	   (unsigned long) prevhbp, (unsigned long) hbp);
      }

    /* Coalesce with successor, if possible */
      if( (((word)p)+size) == ((word)hbp) ) {
	phdr->hb_next = hhdr->hb_next;
	phdr->hb_sz += hhdr->hb_sz;
	GC_remove_header(hbp);
      } else {
	phdr->hb_next = hbp;
      }

    
    if( prevhbp == 0 ) {
	GC_hblkfreelist = p;
    } else if( (((word)prevhbp) + prevhdr->hb_sz)
      	       == ((word)p) ) {
      /* Coalesce with predecessor */
	prevhdr->hb_next = phdr->hb_next;
	prevhdr->hb_sz += phdr->hb_sz;
	GC_remove_header(p);
    } else {
	prevhdr->hb_next = p;
    }
}

