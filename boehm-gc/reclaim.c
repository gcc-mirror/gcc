/* 
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1996 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1999 by Hewlett-Packard Company. All rights reserved.
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

#include <stdio.h>
#include "gc_priv.h"

signed_word GC_mem_found = 0;
			/* Number of words of memory reclaimed     */

static void report_leak(p, sz)
ptr_t p;
word sz;
{
    if (HDR(p) -> hb_obj_kind == PTRFREE) {
        GC_err_printf0("Leaked atomic object at ");
    } else {
        GC_err_printf0("Leaked composite object at ");
    }
    GC_print_heap_obj(p);
    GC_err_printf0("\n");
}

#   define FOUND_FREE(hblk, word_no) \
      { \
         report_leak((ptr_t)hblk + WORDS_TO_BYTES(word_no), \
         	     HDR(hblk) -> hb_sz); \
      }

/*
 * reclaim phase
 *
 */


/*
 * Test whether a block is completely empty, i.e. contains no marked
 * objects.  This does not require the block to be in physical
 * memory.
 */
 
GC_bool GC_block_empty(hhdr)
register hdr * hhdr;
{
    register word *p = (word *)(&(hhdr -> hb_marks[0]));
    register word * plim =
	    		(word *)(&(hhdr -> hb_marks[MARK_BITS_SZ]));
    while (p < plim) {
	if (*p++) return(FALSE);
    }
    return(TRUE);
}

/* The following functions sometimes return a DONT_KNOW value. */
#define DONT_KNOW  2

#ifdef SMALL_CONFIG
# define GC_block_nearly_full1(hhdr, pat1) DONT_KNOW
# define GC_block_nearly_full3(hhdr, pat1, pat2) DONT_KNOW
# define GC_block_nearly_full(hhdr) DONT_KNOW
#else

/*
 * Test whether nearly all of the mark words consist of the same
 * repeating pattern.
 */
#define FULL_THRESHOLD (MARK_BITS_SZ/16)

GC_bool GC_block_nearly_full1(hhdr, pat1)
hdr *hhdr;
word pat1;
{
    unsigned i;
    unsigned misses = 0;
    GC_ASSERT((MARK_BITS_SZ & 1) == 0);
    for (i = 0; i < MARK_BITS_SZ; ++i) {
	if ((hhdr -> hb_marks[i] | ~pat1) != ONES) {
	    if (++misses > FULL_THRESHOLD) return FALSE;
	}
    }
    return TRUE;
}

/*
 * Test whether the same repeating 3 word pattern occurs in nearly
 * all the mark bit slots.
 * This is used as a heuristic, so we're a bit sloppy and ignore
 * the last one or two words.
 */
GC_bool GC_block_nearly_full3(hhdr, pat1, pat2, pat3)
hdr *hhdr;
word pat1, pat2, pat3;
{
    unsigned i;
    unsigned misses = 0;

    if (MARK_BITS_SZ < 4) {
      return DONT_KNOW;
    }
    for (i = 0; i < MARK_BITS_SZ - 2; i += 3) {
	if ((hhdr -> hb_marks[i] | ~pat1) != ONES) {
	    if (++misses > FULL_THRESHOLD) return FALSE;
	}
	if ((hhdr -> hb_marks[i+1] | ~pat2) != ONES) {
	    if (++misses > FULL_THRESHOLD) return FALSE;
	}
	if ((hhdr -> hb_marks[i+2] | ~pat3) != ONES) {
	    if (++misses > FULL_THRESHOLD) return FALSE;
	}
    }
    return TRUE;
}

/* Check whether a small object block is nearly full by looking at only */
/* the mark bits.							*/
/* We manually precomputed the mark bit patterns that need to be 	*/
/* checked for, and we give up on the ones that are unlikely to occur,	*/
/* or have period > 3.							*/
/* This would be a lot easier with a mark bit per object instead of per	*/
/* word, but that would rewuire computing object numbers in the mark	*/
/* loop, which would require different data structures ...		*/
GC_bool GC_block_nearly_full(hhdr)
hdr *hhdr;
{
    int sz = hhdr -> hb_sz;

#   if CPP_WORDSZ != 32 && CPP_WORDSZ != 64
      return DONT_KNOW;	/* Shouldn't be used in any standard config.	*/
#   endif
    if (0 != HDR_WORDS) return DONT_KNOW;
	/* Also shouldn't happen */
#   if CPP_WORDSZ == 32
      switch(sz) {
        case 1:
	  return GC_block_nearly_full1(hhdr, 0xffffffffl);
	case 2:
	  return GC_block_nearly_full1(hhdr, 0x55555555l);
	case 4:
	  return GC_block_nearly_full1(hhdr, 0x11111111l);
	case 6:
	  return GC_block_nearly_full3(hhdr, 0x41041041l,
					      0x10410410l,
					       0x04104104l);
	case 8:
	  return GC_block_nearly_full1(hhdr, 0x01010101l);
	case 12:
	  return GC_block_nearly_full3(hhdr, 0x01001001l,
					      0x10010010l,
					       0x00100100l);
	case 16:
	  return GC_block_nearly_full1(hhdr, 0x00010001l);
	case 32:
	  return GC_block_nearly_full1(hhdr, 0x00000001l);
	default:
	  return DONT_KNOW;
      }
#   endif
#   if CPP_WORDSZ == 64
      switch(sz) {
        case 1:
	  return GC_block_nearly_full1(hhdr, 0xffffffffffffffffl);
	case 2:
	  return GC_block_nearly_full1(hhdr, 0x5555555555555555l);
	case 4:
	  return GC_block_nearly_full1(hhdr, 0x1111111111111111l);
	case 6:
	  return GC_block_nearly_full3(hhdr, 0x1041041041041041l,
					       0x4104104104104104l,
					         0x0410410410410410l);
	case 8:
	  return GC_block_nearly_full1(hhdr, 0x0101010101010101l);
	case 12:
	  return GC_block_nearly_full3(hhdr, 0x1001001001001001l,
					       0x0100100100100100l,
					         0x0010010010010010l);
	case 16:
	  return GC_block_nearly_full1(hhdr, 0x0001000100010001l);
	case 32:
	  return GC_block_nearly_full1(hhdr, 0x0000000100000001l);
	default:
	  return DONT_KNOW;
      }
#   endif
}
#endif /* !SMALL_CONFIG */

# ifdef GATHERSTATS
#   define INCR_WORDS(sz) n_words_found += (sz)
# else
#   define INCR_WORDS(sz)
# endif
/*
 * Restore unmarked small objects in h of size sz to the object
 * free list.  Returns the new list.
 * Clears unmarked objects.
 */
/*ARGSUSED*/
ptr_t GC_reclaim_clear(hbp, hhdr, sz, list)
register struct hblk *hbp;	/* ptr to current heap block		*/
register hdr * hhdr;
register ptr_t list;
register word sz;
{
    register int word_no;
    register word *p, *q, *plim;
#   ifdef GATHERSTATS
        register int n_words_found = 0;
#   endif        
    
    p = (word *)(hbp->hb_body);
    word_no = HDR_WORDS;
    plim = (word *)((((word)hbp) + HBLKSIZE)
		   - WORDS_TO_BYTES(sz));

    /* go through all words in block */
	while( p <= plim )  {
	    if( mark_bit_from_hdr(hhdr, word_no) ) {
		p += sz;
	    } else {
		INCR_WORDS(sz);
		/* object is available - put on list */
		    obj_link(p) = list;
		    list = ((ptr_t)p);
		/* Clear object, advance p to next object in the process */
		    q = p + sz;
                    p++; /* Skip link field */
#		    if defined(SMALL_CONFIG) && defined(ALIGN_DOUBLE)
		      /* We assert that sz must be even	*/
		      *p++ = 0;
		      while (p < q) {
			CLEAR_DOUBLE(p);
			p += 2;
		      }
#		    else
                      while (p < q) {
			*p++ = 0;
		      }
#		    endif
	    }
	    word_no += sz;
	}
#   ifdef GATHERSTATS
	GC_mem_found += n_words_found;
#   endif
    return(list);
}

#ifndef SMALL_CONFIG

/*
 * A special case for 2 word composite objects (e.g. cons cells):
 */
/*ARGSUSED*/
ptr_t GC_reclaim_clear2(hbp, hhdr, list)
register struct hblk *hbp;	/* ptr to current heap block		*/
hdr * hhdr;
register ptr_t list;
{
    register word * mark_word_addr = &(hhdr->hb_marks[divWORDSZ(HDR_WORDS)]);
    register word *p, *plim;
#   ifdef GATHERSTATS
        register int n_words_found = 0;
#   endif
    register word mark_word;
    register int i;
#   define DO_OBJ(start_displ) \
	if (!(mark_word & ((word)1 << start_displ))) { \
	    p[start_displ] = (word)list; \
	    list = (ptr_t)(p+start_displ); \
	    p[start_displ+1] = 0; \
	    INCR_WORDS(2); \
	}
    
    p = (word *)(hbp->hb_body);
    plim = (word *)(((word)hbp) + HBLKSIZE);

    /* go through all words in block */
	while( p < plim )  {
	    mark_word = *mark_word_addr++;
	    for (i = 0; i < WORDSZ; i += 8) {
		DO_OBJ(0);
		DO_OBJ(2);
		DO_OBJ(4);
		DO_OBJ(6);
		p += 8;
		mark_word >>= 8;
	    }
	}	        
#   ifdef GATHERSTATS
	GC_mem_found += n_words_found;
#   endif
    return(list);
#   undef DO_OBJ
}

/*
 * Another special case for 4 word composite objects:
 */
/*ARGSUSED*/
ptr_t GC_reclaim_clear4(hbp, hhdr, list)
register struct hblk *hbp;	/* ptr to current heap block		*/
hdr * hhdr;
register ptr_t list;
{
    register word * mark_word_addr = &(hhdr->hb_marks[divWORDSZ(HDR_WORDS)]);
    register word *p, *plim;
#   ifdef GATHERSTATS
        register int n_words_found = 0;
#   endif
    register word mark_word;
#   define DO_OBJ(start_displ) \
	if (!(mark_word & ((word)1 << start_displ))) { \
	    p[start_displ] = (word)list; \
	    list = (ptr_t)(p+start_displ); \
	    p[start_displ+1] = 0; \
	    CLEAR_DOUBLE(p + start_displ + 2); \
	    INCR_WORDS(4); \
	}
    
    p = (word *)(hbp->hb_body);
    plim = (word *)(((word)hbp) + HBLKSIZE);

    /* go through all words in block */
	while( p < plim )  {
	    mark_word = *mark_word_addr++;
	    DO_OBJ(0);
	    DO_OBJ(4);
	    DO_OBJ(8);
	    DO_OBJ(12);
	    DO_OBJ(16);
	    DO_OBJ(20);
	    DO_OBJ(24);
	    DO_OBJ(28);
#	    if CPP_WORDSZ == 64
	      DO_OBJ(32);
	      DO_OBJ(36);
	      DO_OBJ(40);
	      DO_OBJ(44);
	      DO_OBJ(48);
	      DO_OBJ(52);
	      DO_OBJ(56);
	      DO_OBJ(60);
#	    endif
	    p += WORDSZ;
	}	        
#   ifdef GATHERSTATS
	GC_mem_found += n_words_found;
#   endif
    return(list);
#   undef DO_OBJ
}

#endif /* !SMALL_CONFIG */

/* The same thing, but don't clear objects: */
/*ARGSUSED*/
ptr_t GC_reclaim_uninit(hbp, hhdr, sz, list)
register struct hblk *hbp;	/* ptr to current heap block		*/
register hdr * hhdr;
register ptr_t list;
register word sz;
{
    register int word_no;
    register word *p, *plim;
#   ifdef GATHERSTATS
        register int n_words_found = 0;
#   endif
    
    p = (word *)(hbp->hb_body);
    word_no = HDR_WORDS;
    plim = (word *)((((word)hbp) + HBLKSIZE)
		   - WORDS_TO_BYTES(sz));

    /* go through all words in block */
	while( p <= plim )  {
	    if( !mark_bit_from_hdr(hhdr, word_no) ) {
		INCR_WORDS(sz);
		/* object is available - put on list */
		    obj_link(p) = list;
		    list = ((ptr_t)p);
	    }
	    p += sz;
	    word_no += sz;
	}
#   ifdef GATHERSTATS
	GC_mem_found += n_words_found;
#   endif
    return(list);
}

/* Don't really reclaim objects, just check for unmarked ones: */
/*ARGSUSED*/
void GC_reclaim_check(hbp, hhdr, sz)
register struct hblk *hbp;	/* ptr to current heap block		*/
register hdr * hhdr;
register word sz;
{
    register int word_no;
    register word *p, *plim;
#   ifdef GATHERSTATS
        register int n_words_found = 0;
#   endif
    
    p = (word *)(hbp->hb_body);
    word_no = HDR_WORDS;
    plim = (word *)((((word)hbp) + HBLKSIZE)
		   - WORDS_TO_BYTES(sz));

    /* go through all words in block */
	while( p <= plim )  {
	    if( !mark_bit_from_hdr(hhdr, word_no) ) {
		FOUND_FREE(hbp, word_no);
	    }
	    p += sz;
	    word_no += sz;
	}
}

#ifndef SMALL_CONFIG
/*
 * Another special case for 2 word atomic objects:
 */
/*ARGSUSED*/
ptr_t GC_reclaim_uninit2(hbp, hhdr, list)
register struct hblk *hbp;	/* ptr to current heap block		*/
hdr * hhdr;
register ptr_t list;
{
    register word * mark_word_addr = &(hhdr->hb_marks[divWORDSZ(HDR_WORDS)]);
    register word *p, *plim;
#   ifdef GATHERSTATS
        register int n_words_found = 0;
#   endif
    register word mark_word;
    register int i;
#   define DO_OBJ(start_displ) \
	if (!(mark_word & ((word)1 << start_displ))) { \
	    p[start_displ] = (word)list; \
	    list = (ptr_t)(p+start_displ); \
	    INCR_WORDS(2); \
	}
    
    p = (word *)(hbp->hb_body);
    plim = (word *)(((word)hbp) + HBLKSIZE);

    /* go through all words in block */
	while( p < plim )  {
	    mark_word = *mark_word_addr++;
	    for (i = 0; i < WORDSZ; i += 8) {
		DO_OBJ(0);
		DO_OBJ(2);
		DO_OBJ(4);
		DO_OBJ(6);
		p += 8;
		mark_word >>= 8;
	    }
	}	        
#   ifdef GATHERSTATS
	GC_mem_found += n_words_found;
#   endif
    return(list);
#   undef DO_OBJ
}

/*
 * Another special case for 4 word atomic objects:
 */
/*ARGSUSED*/
ptr_t GC_reclaim_uninit4(hbp, hhdr, list)
register struct hblk *hbp;	/* ptr to current heap block		*/
hdr * hhdr;
register ptr_t list;
{
    register word * mark_word_addr = &(hhdr->hb_marks[divWORDSZ(HDR_WORDS)]);
    register word *p, *plim;
#   ifdef GATHERSTATS
        register int n_words_found = 0;
#   endif
    register word mark_word;
#   define DO_OBJ(start_displ) \
	if (!(mark_word & ((word)1 << start_displ))) { \
	    p[start_displ] = (word)list; \
	    list = (ptr_t)(p+start_displ); \
	    INCR_WORDS(4); \
	}
    
    p = (word *)(hbp->hb_body);
    plim = (word *)(((word)hbp) + HBLKSIZE);

    /* go through all words in block */
	while( p < plim )  {
	    mark_word = *mark_word_addr++;
	    DO_OBJ(0);
	    DO_OBJ(4);
	    DO_OBJ(8);
	    DO_OBJ(12);
	    DO_OBJ(16);
	    DO_OBJ(20);
	    DO_OBJ(24);
	    DO_OBJ(28);
#	    if CPP_WORDSZ == 64
	      DO_OBJ(32);
	      DO_OBJ(36);
	      DO_OBJ(40);
	      DO_OBJ(44);
	      DO_OBJ(48);
	      DO_OBJ(52);
	      DO_OBJ(56);
	      DO_OBJ(60);
#	    endif
	    p += WORDSZ;
	}	        
#   ifdef GATHERSTATS
	GC_mem_found += n_words_found;
#   endif
    return(list);
#   undef DO_OBJ
}

/* Finally the one word case, which never requires any clearing: */
/*ARGSUSED*/
ptr_t GC_reclaim1(hbp, hhdr, list)
register struct hblk *hbp;	/* ptr to current heap block		*/
hdr * hhdr;
register ptr_t list;
{
    register word * mark_word_addr = &(hhdr->hb_marks[divWORDSZ(HDR_WORDS)]);
    register word *p, *plim;
#   ifdef GATHERSTATS
        register int n_words_found = 0;
#   endif
    register word mark_word;
    register int i;
#   define DO_OBJ(start_displ) \
	if (!(mark_word & ((word)1 << start_displ))) { \
	    p[start_displ] = (word)list; \
	    list = (ptr_t)(p+start_displ); \
	    INCR_WORDS(1); \
	}
    
    p = (word *)(hbp->hb_body);
    plim = (word *)(((word)hbp) + HBLKSIZE);

    /* go through all words in block */
	while( p < plim )  {
	    mark_word = *mark_word_addr++;
	    for (i = 0; i < WORDSZ; i += 4) {
		DO_OBJ(0);
		DO_OBJ(1);
		DO_OBJ(2);
		DO_OBJ(3);
		p += 4;
		mark_word >>= 4;
	    }
	}	        
#   ifdef GATHERSTATS
	GC_mem_found += n_words_found;
#   endif
    return(list);
#   undef DO_OBJ
}

#endif /* !SMALL_CONFIG */

/*
 * Restore unmarked small objects in the block pointed to by hbp
 * to the appropriate object free list.
 * If entirely empty blocks are to be completely deallocated, then
 * caller should perform that check.
 */
void GC_reclaim_small_nonempty_block(hbp, report_if_found)
register struct hblk *hbp;	/* ptr to current heap block		*/
int report_if_found;		/* Abort if a reclaimable object is found */
{
    hdr * hhdr;
    word sz;		/* size of objects in current block	*/
    struct obj_kind * ok;
    ptr_t * flh;
    int kind;
    GC_bool full;
    
    hhdr = HDR(hbp);
    sz = hhdr -> hb_sz;
    hhdr -> hb_last_reclaimed = (unsigned short) GC_gc_no;
    kind = hhdr -> hb_obj_kind;
    ok = &GC_obj_kinds[kind];
    flh = &(ok -> ok_freelist[sz]);

    if (report_if_found) {
	GC_reclaim_check(hbp, hhdr, sz);
    } else if (ok -> ok_init) {
      switch(sz) {
#      ifndef SMALL_CONFIG
        case 1:
	    full = GC_block_nearly_full1(hhdr, 0xffffffffl);
	    if (TRUE == full) goto out;
	    if (FALSE == full) GC_write_hint(hbp);
	    /* In the DONT_KNOW case, we let reclaim fault.	*/
            *flh = GC_reclaim1(hbp, hhdr, *flh);
            break;
        case 2:
	    full = GC_block_nearly_full1(hhdr, 0x55555555l);
	    if (TRUE == full) goto out;
	    if (FALSE == full) GC_write_hint(hbp);
            *flh = GC_reclaim_clear2(hbp, hhdr, *flh);
            break;
        case 4:
	    full = GC_block_nearly_full1(hhdr, 0x11111111l);
	    if (TRUE == full) goto out;
	    if (FALSE == full) GC_write_hint(hbp);
            *flh = GC_reclaim_clear4(hbp, hhdr, *flh);
            break;
#      endif
        default:
	    full = GC_block_nearly_full(hhdr);
	    if (TRUE == full) goto out;
	    if (FALSE == full) GC_write_hint(hbp);
            *flh = GC_reclaim_clear(hbp, hhdr, sz, *flh);
            break;
      }
    } else {
      switch(sz) {
#      ifndef SMALL_CONFIG
        case 1:
	    full = GC_block_nearly_full1(hhdr, 0xffffffffl);
	    if (TRUE == full) goto out;
	    if (FALSE == full) GC_write_hint(hbp);
            *flh = GC_reclaim1(hbp, hhdr, *flh);
            break;
        case 2:
	    full = GC_block_nearly_full1(hhdr, 0x55555555l);
	    if (TRUE == full) goto out;
	    if (FALSE == full) GC_write_hint(hbp);
            *flh = GC_reclaim_uninit2(hbp, hhdr, *flh);
            break;
        case 4:
	    full = GC_block_nearly_full1(hhdr, 0x11111111l);
	    if (TRUE == full) goto out;
	    if (FALSE == full) GC_write_hint(hbp);
            *flh = GC_reclaim_uninit4(hbp, hhdr, *flh);
            break;
#      endif
        default:
	    full = GC_block_nearly_full(hhdr);
	    if (TRUE == full) goto out;
	    if (FALSE == full) GC_write_hint(hbp);
            *flh = GC_reclaim_uninit(hbp, hhdr, sz, *flh);
            break;
      }
    } 
out:
    if (IS_UNCOLLECTABLE(kind)) GC_set_hdr_marks(hhdr);
}

/*
 * Restore an unmarked large object or an entirely empty blocks of small objects
 * to the heap block free list.
 * Otherwise enqueue the block for later processing
 * by GC_reclaim_small_nonempty_block.
 * If report_if_found is TRUE, then process any block immediately, and
 * simply report free objects; do not actually reclaim them.
 */
void GC_reclaim_block(hbp, report_if_found)
register struct hblk *hbp;	/* ptr to current heap block		*/
word report_if_found;		/* Abort if a reclaimable object is found */
{
    register hdr * hhdr;
    register word sz;		/* size of objects in current block	*/
    register struct obj_kind * ok;
    struct hblk ** rlh;

    hhdr = HDR(hbp);
    sz = hhdr -> hb_sz;
    ok = &GC_obj_kinds[hhdr -> hb_obj_kind];

    if( sz > MAXOBJSZ ) {  /* 1 big object */
        if( !mark_bit_from_hdr(hhdr, HDR_WORDS) ) {
	    if (report_if_found) {
	      FOUND_FREE(hbp, HDR_WORDS);
	    } else {
#	      ifdef GATHERSTATS
	        GC_mem_found += sz;
#	      endif
	      GC_freehblk(hbp);
	    }
	}
    } else {
        GC_bool empty = GC_block_empty(hhdr);
        if (report_if_found) {
    	  GC_reclaim_small_nonempty_block(hbp, (int)report_if_found);
        } else if (empty) {
#	  ifdef GATHERSTATS
            GC_mem_found += BYTES_TO_WORDS(HBLKSIZE);
#	  endif
          GC_freehblk(hbp);
        } else {
          /* group of smaller objects, enqueue the real work */
          rlh = &(ok -> ok_reclaim_list[sz]);
          hhdr -> hb_next = *rlh;
          *rlh = hbp;
        }
    }
}

#if !defined(NO_DEBUGGING)
/* Routines to gather and print heap block info 	*/
/* intended for debugging.  Otherwise should be called	*/
/* with lock.						*/
static size_t number_of_blocks;
static size_t total_bytes;

/* Number of set bits in a word.  Not performance critical.	*/
static int set_bits(n)
word n;
{
    register word m = n;
    register int result = 0;
    
    while (m > 0) {
    	if (m & 1) result++;
    	m >>= 1;
    }
    return(result);
}

/* Return the number of set mark bits in the given header	*/
int GC_n_set_marks(hhdr)
hdr * hhdr;
{
    register int result = 0;
    register int i;
    
    for (i = 0; i < MARK_BITS_SZ; i++) {
        result += set_bits(hhdr -> hb_marks[i]);
    }
    return(result);
}

/*ARGSUSED*/
void GC_print_block_descr(h, dummy)
struct hblk *h;
word dummy;
{
    register hdr * hhdr = HDR(h);
    register size_t bytes = WORDS_TO_BYTES(hhdr -> hb_sz);
    
    GC_printf3("(%lu:%lu,%lu)", (unsigned long)(hhdr -> hb_obj_kind),
    			        (unsigned long)bytes,
    			        (unsigned long)(GC_n_set_marks(hhdr)));
    bytes += HDR_BYTES + HBLKSIZE-1;
    bytes &= ~(HBLKSIZE-1);
    total_bytes += bytes;
    number_of_blocks++;
}

void GC_print_block_list()
{
    GC_printf0("(kind(0=ptrfree,1=normal,2=unc.,3=stubborn):size_in_bytes, #_marks_set)\n");
    number_of_blocks = 0;
    total_bytes = 0;
    GC_apply_to_all_blocks(GC_print_block_descr, (word)0);
    GC_printf2("\nblocks = %lu, bytes = %lu\n",
    	       (unsigned long)number_of_blocks,
    	       (unsigned long)total_bytes);
}

#endif /* NO_DEBUGGING */

/*
 * Perform GC_reclaim_block on the entire heap, after first clearing
 * small object free lists (if we are not just looking for leaks).
 */
void GC_start_reclaim(report_if_found)
int report_if_found;		/* Abort if a GC_reclaimable object is found */
{
    int kind;
    
    /* Clear reclaim- and free-lists */
      for (kind = 0; kind < GC_n_kinds; kind++) {
        register ptr_t *fop;
        register ptr_t *lim;
        register struct hblk ** rlp;
        register struct hblk ** rlim;
        register struct hblk ** rlist = GC_obj_kinds[kind].ok_reclaim_list;
        
        if (rlist == 0) continue;	/* This kind not used.	*/
        if (!report_if_found) {
            lim = &(GC_obj_kinds[kind].ok_freelist[MAXOBJSZ+1]);
	    for( fop = GC_obj_kinds[kind].ok_freelist; fop < lim; fop++ ) {
	      *fop = 0;
	    }
	} /* otherwise free list objects are marked, 	*/
	  /* and its safe to leave them			*/
	rlim = rlist + MAXOBJSZ+1;
	for( rlp = rlist; rlp < rlim; rlp++ ) {
	    *rlp = 0;
	}
      }
    
#   ifdef PRINTBLOCKS
        GC_printf0("GC_reclaim: current block sizes:\n");
        GC_print_block_list();
#   endif

  /* Go through all heap blocks (in hblklist) and reclaim unmarked objects */
  /* or enqueue the block for later processing.				   */
    GC_apply_to_all_blocks(GC_reclaim_block, (word)report_if_found);

# ifdef EAGER_SWEEP
    /* This is a very stupid thing to do.  We make it possible anyway,	*/
    /* so that you can convince yourself that it really is very stupid.	*/
    GC_reclaim_all((GC_stop_func)0, FALSE);
# endif
    
}

/*
 * Sweep blocks of the indicated object size and kind until either the
 * appropriate free list is nonempty, or there are no more blocks to
 * sweep.
 */
void GC_continue_reclaim(sz, kind)
word sz;	/* words */
int kind;
{
    register hdr * hhdr;
    register struct hblk * hbp;
    register struct obj_kind * ok = &(GC_obj_kinds[kind]);
    struct hblk ** rlh = ok -> ok_reclaim_list;
    ptr_t *flh = &(ok -> ok_freelist[sz]);
    
    if (rlh == 0) return;	/* No blocks of this kind.	*/
    rlh += sz;
    while ((hbp = *rlh) != 0) {
        hhdr = HDR(hbp);
        *rlh = hhdr -> hb_next;
        GC_reclaim_small_nonempty_block(hbp, FALSE);
        if (*flh != 0) break;
    }
}

/*
 * Reclaim all small blocks waiting to be reclaimed.
 * Abort and return FALSE when/if (*stop_func)() returns TRUE.
 * If this returns TRUE, then it's safe to restart the world
 * with incorrectly cleared mark bits.
 * If ignore_old is TRUE, then reclaim only blocks that have been 
 * recently reclaimed, and discard the rest.
 * Stop_func may be 0.
 */
GC_bool GC_reclaim_all(stop_func, ignore_old)
GC_stop_func stop_func;
GC_bool ignore_old;
{
    register word sz;
    register int kind;
    register hdr * hhdr;
    register struct hblk * hbp;
    register struct obj_kind * ok;
    struct hblk ** rlp;
    struct hblk ** rlh;
#   ifdef PRINTTIMES
	CLOCK_TYPE start_time;
	CLOCK_TYPE done_time;
	
	GET_TIME(start_time);
#   endif
    
    for (kind = 0; kind < GC_n_kinds; kind++) {
    	ok = &(GC_obj_kinds[kind]);
    	rlp = ok -> ok_reclaim_list;
    	if (rlp == 0) continue;
    	for (sz = 1; sz <= MAXOBJSZ; sz++) {
    	    rlh = rlp + sz;
    	    while ((hbp = *rlh) != 0) {
    	        if (stop_func != (GC_stop_func)0 && (*stop_func)()) {
    	            return(FALSE);
    	        }
        	hhdr = HDR(hbp);
        	*rlh = hhdr -> hb_next;
        	if (!ignore_old || hhdr -> hb_last_reclaimed == GC_gc_no - 1) {
        	    /* It's likely we'll need it this time, too	*/
        	    /* It's been touched recently, so this	*/
        	    /* shouldn't trigger paging.		*/
        	    GC_reclaim_small_nonempty_block(hbp, FALSE);
        	}
            }
        }
    }
#   ifdef PRINTTIMES
	GET_TIME(done_time);
	GC_printf1("Disposing of reclaim lists took %lu msecs\n",
	           MS_TIME_DIFF(done_time,start_time));
#   endif
    return(TRUE);
}
