/* 
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1995 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1997 by Silicon Graphics.  All rights reserved.
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
 */

#include "dbg_mlc.h"

void GC_default_print_heap_obj_proc();
GC_API void GC_register_finalizer_no_order
    	GC_PROTO((GC_PTR obj, GC_finalization_proc fn, GC_PTR cd,
		  GC_finalization_proc *ofn, GC_PTR *ocd));


/* Check whether object with base pointer p has debugging info	*/ 
/* p is assumed to point to a legitimate object in our part	*/
/* of the heap.							*/
GC_bool GC_has_debug_info(p)
ptr_t p;
{
    register oh * ohdr = (oh *)p;
    register ptr_t body = (ptr_t)(ohdr + 1);
    register word sz = GC_size((ptr_t) ohdr);
    
    if (HBLKPTR((ptr_t)ohdr) != HBLKPTR((ptr_t)body)
        || sz < sizeof (oh)) {
        return(FALSE);
    }
    if (ohdr -> oh_sz == sz) {
    	/* Object may have had debug info, but has been deallocated	*/
    	return(FALSE);
    }
    if (ohdr -> oh_sf == (START_FLAG ^ (word)body)) return(TRUE);
    if (((word *)ohdr)[BYTES_TO_WORDS(sz)-1] == (END_FLAG ^ (word)body)) {
        return(TRUE);
    }
    return(FALSE);
}

#ifdef KEEP_BACK_PTRS
  /* Store back pointer to source in dest, if that appears to be possible. */
  /* This is not completely safe, since we may mistakenly conclude that	   */
  /* dest has a debugging wrapper.  But the error probability is very	   */
  /* small, and this shouldn't be used in production code.		   */
  /* We assume that dest is the real base pointer.  Source will usually    */
  /* be a pointer to the interior of an object.				   */
  void GC_store_back_pointer(ptr_t source, ptr_t dest)
  {
    if (GC_has_debug_info(dest)) {
      ((oh *)dest) -> oh_back_ptr = (ptr_t)HIDE_POINTER(source);
    }
  }

  void GC_marked_for_finalization(ptr_t dest) {
    GC_store_back_pointer(MARKED_FOR_FINALIZATION, dest);
  }

  /* Store information about the object referencing dest in *base_p	*/
  /* and *offset_p.							*/
  /*   source is root ==> *base_p = address, *offset_p = 0		*/
  /*   source is heap object ==> *base_p != 0, *offset_p = offset 	*/
  /*   Returns 1 on success, 0 if source couldn't be determined.	*/
  /* Dest can be any address within a heap object.			*/
  GC_ref_kind GC_get_back_ptr_info(void *dest, void **base_p, size_t *offset_p)
  {
    oh * hdr = (oh *)GC_base(dest);
    ptr_t bp;
    ptr_t bp_base;
    if (!GC_has_debug_info((ptr_t) hdr)) return GC_NO_SPACE;
    bp = hdr -> oh_back_ptr;
    if (MARKED_FOR_FINALIZATION == bp) return GC_FINALIZER_REFD;
    if (MARKED_FROM_REGISTER == bp) return GC_REFD_FROM_REG;
    if (0 == bp) return GC_UNREFERENCED;
    bp = REVEAL_POINTER(bp);
    bp_base = GC_base(bp);
    if (0 == bp_base) {
      *base_p = bp;
      *offset_p = 0;
      return GC_REFD_FROM_ROOT;
    } else {
      if (GC_has_debug_info(bp_base)) bp_base += sizeof(oh);
      *base_p = bp_base;
      *offset_p = bp - bp_base;
      return GC_REFD_FROM_HEAP;
    }
  }

  /* Generate a random heap address.		*/
  /* The resulting address is in the heap, but	*/
  /* not necessarily inside a valid object.	*/
  void *GC_generate_random_heap_address(void)
  {
    int i;
    int heap_offset = random() % GC_heapsize;
    for (i = 0; i < GC_n_heap_sects; ++ i) {
	int size = GC_heap_sects[i].hs_bytes;
	if (heap_offset < size) {
	    return GC_heap_sects[i].hs_start + heap_offset;
	} else {
	    heap_offset -= size;
	}
    }
    ABORT("GC_generate_random_heap_address: size inconsistency");
    /*NOTREACHED*/
    return 0;
  }

  /* Generate a random address inside a valid marked heap object. */
  void *GC_generate_random_valid_address(void)
  {
    ptr_t result;
    ptr_t base;
    for (;;) {
	result = GC_generate_random_heap_address();
  	base = GC_base(result);
	if (0 == base) continue;
	if (!GC_is_marked(base)) continue;
	return result;
    }
  }

  /* Print back trace for p */
  void GC_print_backtrace(void *p)
  {
    void *current = p;
    int i;
    GC_ref_kind source;
    size_t offset;
    void *base;

    GC_print_heap_obj(GC_base(current));
    GC_err_printf0("\n");
    for (i = 0; ; ++i) {
      source = GC_get_back_ptr_info(current, &base, &offset);
      if (GC_UNREFERENCED == source) {
	GC_err_printf0("Reference could not be found\n");
  	goto out;
      }
      if (GC_NO_SPACE == source) {
	GC_err_printf0("No debug info in object: Can't find reference\n");
	goto out;
      }
      GC_err_printf1("Reachable via %d levels of pointers from ",
		 (unsigned long)i);
      switch(source) {
	case GC_REFD_FROM_ROOT:
	  GC_err_printf1("root at 0x%lx\n", (unsigned long)base);
	  goto out;
	case GC_REFD_FROM_REG:
	  GC_err_printf0("root in register\n");
	  goto out;
	case GC_FINALIZER_REFD:
	  GC_err_printf0("list of finalizable objects\n");
	  goto out;
	case GC_REFD_FROM_HEAP:
	  GC_err_printf1("offset %ld in object:\n", (unsigned long)offset);
	  /* Take GC_base(base) to get real base, i.e. header. */
	  GC_print_heap_obj(GC_base(base));
	  GC_err_printf0("\n");
	  break;
      }
      current = base;
    }
    out:;
  }

  /* Force a garbage collection and generate a backtrace from a	*/
  /* random heap address.					*/
  void GC_generate_random_backtrace(void)
  {
    void * current;
    GC_gcollect();
    current = GC_generate_random_valid_address();
    GC_printf1("Chose address 0x%lx in object\n", (unsigned long)current);
    GC_print_backtrace(current);
  }
    
#endif /* KEEP_BACK_PTRS */

/* Store debugging info into p.  Return displaced pointer. */
/* Assumes we don't hold allocation lock.		   */
ptr_t GC_store_debug_info(p, sz, string, integer)
register ptr_t p;	/* base pointer */
word sz; 	/* bytes */
char * string;
word integer;
{
    register word * result = (word *)((oh *)p + 1);
    DCL_LOCK_STATE;
    
    /* There is some argument that we should dissble signals here.	*/
    /* But that's expensive.  And this way things should only appear	*/
    /* inconsistent while we're in the handler.				*/
    LOCK();
#   ifdef KEEP_BACK_PTRS
      ((oh *)p) -> oh_back_ptr = 0;
#   endif
    ((oh *)p) -> oh_string = string;
    ((oh *)p) -> oh_int = integer;
    ((oh *)p) -> oh_sz = sz;
    ((oh *)p) -> oh_sf = START_FLAG ^ (word)result;
    ((word *)p)[BYTES_TO_WORDS(GC_size(p))-1] =
         result[ROUNDED_UP_WORDS(sz)] = END_FLAG ^ (word)result;
    UNLOCK();
    return((ptr_t)result);
}

/* Check the object with debugging info at ohdr		*/
/* return NIL if it's OK.  Else return clobbered	*/
/* address.						*/
ptr_t GC_check_annotated_obj(ohdr)
register oh * ohdr;
{
    register ptr_t body = (ptr_t)(ohdr + 1);
    register word gc_sz = GC_size((ptr_t)ohdr);
    if (ohdr -> oh_sz + DEBUG_BYTES > gc_sz) {
        return((ptr_t)(&(ohdr -> oh_sz)));
    }
    if (ohdr -> oh_sf != (START_FLAG ^ (word)body)) {
        return((ptr_t)(&(ohdr -> oh_sf)));
    }
    if (((word *)ohdr)[BYTES_TO_WORDS(gc_sz)-1] != (END_FLAG ^ (word)body)) {
        return((ptr_t)((word *)ohdr + BYTES_TO_WORDS(gc_sz)-1));
    }
    if (((word *)body)[ROUNDED_UP_WORDS(ohdr -> oh_sz)]
        != (END_FLAG ^ (word)body)) {
        return((ptr_t)((word *)body + ROUNDED_UP_WORDS(ohdr -> oh_sz)));
    }
    return(0);
}

void GC_print_obj(p)
ptr_t p;
{
    register oh * ohdr = (oh *)GC_base(p);
    
    GC_err_printf1("0x%lx (", ((unsigned long)ohdr + sizeof(oh)));
    GC_err_puts(ohdr -> oh_string);
    GC_err_printf2(":%ld, sz=%ld)\n", (unsigned long)(ohdr -> oh_int),
        			      (unsigned long)(ohdr -> oh_sz));
    PRINT_CALL_CHAIN(ohdr);
}

void GC_debug_print_heap_obj_proc(p)
ptr_t p;
{
    if (GC_has_debug_info(p)) {
	GC_print_obj(p);
    } else {
	GC_default_print_heap_obj_proc(p);
    }
}

void GC_print_smashed_obj(p, clobbered_addr)
ptr_t p, clobbered_addr;
{
    register oh * ohdr = (oh *)GC_base(p);
    
    GC_err_printf2("0x%lx in object at 0x%lx(", (unsigned long)clobbered_addr,
    					        (unsigned long)p);
    if (clobbered_addr <= (ptr_t)(&(ohdr -> oh_sz))
        || ohdr -> oh_string == 0) {
        GC_err_printf1("<smashed>, appr. sz = %ld)\n",
        	       (GC_size((ptr_t)ohdr) - DEBUG_BYTES));
    } else {
        if (ohdr -> oh_string[0] == '\0') {
            GC_err_puts("EMPTY(smashed?)");
        } else {
            GC_err_puts(ohdr -> oh_string);
        }
        GC_err_printf2(":%ld, sz=%ld)\n", (unsigned long)(ohdr -> oh_int),
        			          (unsigned long)(ohdr -> oh_sz));
        PRINT_CALL_CHAIN(ohdr);
    }
}

void GC_check_heap_proc();

void GC_start_debugging()
{
    GC_check_heap = GC_check_heap_proc;
    GC_print_heap_obj = GC_debug_print_heap_obj_proc;
    GC_debugging_started = TRUE;
    GC_register_displacement((word)sizeof(oh));
}

# if defined(__STDC__) || defined(__cplusplus)
    void GC_debug_register_displacement(GC_word offset)
# else
    void GC_debug_register_displacement(offset) 
    GC_word offset;
# endif
{
    GC_register_displacement(offset);
    GC_register_displacement((word)sizeof(oh) + offset);
}

# ifdef __STDC__
    GC_PTR GC_debug_malloc(size_t lb, GC_EXTRA_PARAMS)
# else
    GC_PTR GC_debug_malloc(lb, s, i)
    size_t lb;
    char * s;
    int i;
#   ifdef GC_ADD_CALLER
	--> GC_ADD_CALLER not implemented for K&R C
#   endif
# endif
{
    GC_PTR result = GC_malloc(lb + DEBUG_BYTES);
    
    if (result == 0) {
        GC_err_printf1("GC_debug_malloc(%ld) returning NIL (",
        	       (unsigned long) lb);
        GC_err_puts(s);
        GC_err_printf1(":%ld)\n", (unsigned long)i);
        return(0);
    }
    if (!GC_debugging_started) {
    	GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (GC_store_debug_info(result, (word)lb, s, (word)i));
}

# ifdef __STDC__
    GC_PTR GC_debug_generic_malloc(size_t lb, int k, GC_EXTRA_PARAMS)
# else
    GC_PTR GC_debug_malloc(lb, k, s, i)
    size_t lb;
    int k;
    char * s;
    int i;
#   ifdef GC_ADD_CALLER
	--> GC_ADD_CALLER not implemented for K&R C
#   endif
# endif
{
    GC_PTR result = GC_generic_malloc(lb + DEBUG_BYTES, k);
    
    if (result == 0) {
        GC_err_printf1("GC_debug_malloc(%ld) returning NIL (",
        	       (unsigned long) lb);
        GC_err_puts(s);
        GC_err_printf1(":%ld)\n", (unsigned long)i);
        return(0);
    }
    if (!GC_debugging_started) {
    	GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (GC_store_debug_info(result, (word)lb, s, (word)i));
}

#ifdef STUBBORN_ALLOC
# ifdef __STDC__
    GC_PTR GC_debug_malloc_stubborn(size_t lb, GC_EXTRA_PARAMS)
# else
    GC_PTR GC_debug_malloc_stubborn(lb, s, i)
    size_t lb;
    char * s;
    int i;
# endif
{
    GC_PTR result = GC_malloc_stubborn(lb + DEBUG_BYTES);
    
    if (result == 0) {
        GC_err_printf1("GC_debug_malloc(%ld) returning NIL (",
        	       (unsigned long) lb);
        GC_err_puts(s);
        GC_err_printf1(":%ld)\n", (unsigned long)i);
        return(0);
    }
    if (!GC_debugging_started) {
    	GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (GC_store_debug_info(result, (word)lb, s, (word)i));
}

void GC_debug_change_stubborn(p)
GC_PTR p;
{
    register GC_PTR q = GC_base(p);
    register hdr * hhdr;
    
    if (q == 0) {
        GC_err_printf1("Bad argument: 0x%lx to GC_debug_change_stubborn\n",
        	       (unsigned long) p);
        ABORT("GC_debug_change_stubborn: bad arg");
    }
    hhdr = HDR(q);
    if (hhdr -> hb_obj_kind != STUBBORN) {
        GC_err_printf1("GC_debug_change_stubborn arg not stubborn: 0x%lx\n",
        	       (unsigned long) p);
        ABORT("GC_debug_change_stubborn: arg not stubborn");
    }
    GC_change_stubborn(q);
}

void GC_debug_end_stubborn_change(p)
GC_PTR p;
{
    register GC_PTR q = GC_base(p);
    register hdr * hhdr;
    
    if (q == 0) {
        GC_err_printf1("Bad argument: 0x%lx to GC_debug_end_stubborn_change\n",
        	       (unsigned long) p);
        ABORT("GC_debug_end_stubborn_change: bad arg");
    }
    hhdr = HDR(q);
    if (hhdr -> hb_obj_kind != STUBBORN) {
        GC_err_printf1("debug_end_stubborn_change arg not stubborn: 0x%lx\n",
        	       (unsigned long) p);
        ABORT("GC_debug_end_stubborn_change: arg not stubborn");
    }
    GC_end_stubborn_change(q);
}

#else /* !STUBBORN_ALLOC */

# ifdef __STDC__
    GC_PTR GC_debug_malloc_stubborn(size_t lb, GC_EXTRA_PARAMS)
# else
    GC_PTR GC_debug_malloc_stubborn(lb, s, i)
    size_t lb;
    char * s;
    int i;
# endif
{
    return GC_debug_malloc(lb, OPT_RA s, i);
}

void GC_debug_change_stubborn(p)
GC_PTR p;
{
}

void GC_debug_end_stubborn_change(p)
GC_PTR p;
{
}

#endif /* !STUBBORN_ALLOC */

# ifdef __STDC__
    GC_PTR GC_debug_malloc_atomic(size_t lb, GC_EXTRA_PARAMS)
# else
    GC_PTR GC_debug_malloc_atomic(lb, s, i)
    size_t lb;
    char * s;
    int i;
# endif
{
    GC_PTR result = GC_malloc_atomic(lb + DEBUG_BYTES);
    
    if (result == 0) {
        GC_err_printf1("GC_debug_malloc_atomic(%ld) returning NIL (",
        	      (unsigned long) lb);
        GC_err_puts(s);
        GC_err_printf1(":%ld)\n", (unsigned long)i);
        return(0);
    }
    if (!GC_debugging_started) {
        GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (GC_store_debug_info(result, (word)lb, s, (word)i));
}

# ifdef __STDC__
    GC_PTR GC_debug_malloc_uncollectable(size_t lb, GC_EXTRA_PARAMS)
# else
    GC_PTR GC_debug_malloc_uncollectable(lb, s, i)
    size_t lb;
    char * s;
    int i;
# endif
{
    GC_PTR result = GC_malloc_uncollectable(lb + DEBUG_BYTES);
    
    if (result == 0) {
        GC_err_printf1("GC_debug_malloc_uncollectable(%ld) returning NIL (",
        	      (unsigned long) lb);
        GC_err_puts(s);
        GC_err_printf1(":%ld)\n", (unsigned long)i);
        return(0);
    }
    if (!GC_debugging_started) {
        GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (GC_store_debug_info(result, (word)lb, s, (word)i));
}

#ifdef ATOMIC_UNCOLLECTABLE
# ifdef __STDC__
    GC_PTR GC_debug_malloc_atomic_uncollectable(size_t lb, GC_EXTRA_PARAMS)
# else
    GC_PTR GC_debug_malloc_atomic_uncollectable(lb, s, i)
    size_t lb;
    char * s;
    int i;
# endif
{
    GC_PTR result = GC_malloc_atomic_uncollectable(lb + DEBUG_BYTES);
    
    if (result == 0) {
        GC_err_printf1(
		"GC_debug_malloc_atomic_uncollectable(%ld) returning NIL (",
                (unsigned long) lb);
        GC_err_puts(s);
        GC_err_printf1(":%ld)\n", (unsigned long)i);
        return(0);
    }
    if (!GC_debugging_started) {
        GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (GC_store_debug_info(result, (word)lb, s, (word)i));
}
#endif /* ATOMIC_UNCOLLECTABLE */

# ifdef __STDC__
    void GC_debug_free(GC_PTR p)
# else
    void GC_debug_free(p)
    GC_PTR p;
# endif
{
    register GC_PTR base;
    register ptr_t clobbered;
    
    if (0 == p) return;
    base = GC_base(p);
    if (base == 0) {
        GC_err_printf1("Attempt to free invalid pointer %lx\n",
        	       (unsigned long)p);
        ABORT("free(invalid pointer)");
    }
    if ((ptr_t)p - (ptr_t)base != sizeof(oh)) {
        GC_err_printf1(
        	  "GC_debug_free called on pointer %lx wo debugging info\n",
        	  (unsigned long)p);
    } else {
      clobbered = GC_check_annotated_obj((oh *)base);
      if (clobbered != 0) {
        if (((oh *)base) -> oh_sz == GC_size(base)) {
            GC_err_printf0(
                  "GC_debug_free: found previously deallocated (?) object at ");
        } else {
            GC_err_printf0("GC_debug_free: found smashed location at ");
        }
        GC_print_smashed_obj(p, clobbered);
      }
      /* Invalidate size */
      ((oh *)base) -> oh_sz = GC_size(base);
    }
    if (GC_find_leak) {
        GC_free(base);
    } else {
	register hdr * hhdr = HDR(p);
	GC_bool uncollectable = FALSE;

        if (hhdr ->  hb_obj_kind == UNCOLLECTABLE) {
	    uncollectable = TRUE;
	}
#	ifdef ATOMIC_UNCOLLECTABLE
	    if (hhdr ->  hb_obj_kind == AUNCOLLECTABLE) {
		    uncollectable = TRUE;
	    }
#	endif
	if (uncollectable) GC_free(base);
    } /* !GC_find_leak */
}

# ifdef __STDC__
    GC_PTR GC_debug_realloc(GC_PTR p, size_t lb, GC_EXTRA_PARAMS)
# else
    GC_PTR GC_debug_realloc(p, lb, s, i)
    GC_PTR p;
    size_t lb;
    char *s;
    int i;
# endif
{
    register GC_PTR base = GC_base(p);
    register ptr_t clobbered;
    register GC_PTR result;
    register size_t copy_sz = lb;
    register size_t old_sz;
    register hdr * hhdr;
    
    if (p == 0) return(GC_debug_malloc(lb, OPT_RA s, i));
    if (base == 0) {
        GC_err_printf1(
              "Attempt to reallocate invalid pointer %lx\n", (unsigned long)p);
        ABORT("realloc(invalid pointer)");
    }
    if ((ptr_t)p - (ptr_t)base != sizeof(oh)) {
        GC_err_printf1(
        	"GC_debug_realloc called on pointer %lx wo debugging info\n",
        	(unsigned long)p);
        return(GC_realloc(p, lb));
    }
    hhdr = HDR(base);
    switch (hhdr -> hb_obj_kind) {
#    ifdef STUBBORN_ALLOC
      case STUBBORN:
        result = GC_debug_malloc_stubborn(lb, OPT_RA s, i);
        break;
#    endif
      case NORMAL:
        result = GC_debug_malloc(lb, OPT_RA s, i);
        break;
      case PTRFREE:
        result = GC_debug_malloc_atomic(lb, OPT_RA s, i);
        break;
      case UNCOLLECTABLE:
	result = GC_debug_malloc_uncollectable(lb, OPT_RA s, i);
 	break;
#    ifdef ATOMIC_UNCOLLECTABLE
      case AUNCOLLECTABLE:
	result = GC_debug_malloc_atomic_uncollectable(lb, OPT_RA s, i);
	break;
#    endif
      default:
        GC_err_printf0("GC_debug_realloc: encountered bad kind\n");
        ABORT("bad kind");
    }
    clobbered = GC_check_annotated_obj((oh *)base);
    if (clobbered != 0) {
        GC_err_printf0("GC_debug_realloc: found smashed location at ");
        GC_print_smashed_obj(p, clobbered);
    }
    old_sz = ((oh *)base) -> oh_sz;
    if (old_sz < copy_sz) copy_sz = old_sz;
    if (result == 0) return(0);
    BCOPY(p, result,  copy_sz);
    GC_debug_free(p);
    return(result);
}

/* Check all marked objects in the given block for validity */
/*ARGSUSED*/
void GC_check_heap_block(hbp, dummy)
register struct hblk *hbp;	/* ptr to current heap block		*/
word dummy;
{
    register struct hblkhdr * hhdr = HDR(hbp);
    register word sz = hhdr -> hb_sz;
    register int word_no;
    register word *p, *plim;
    
    p = (word *)(hbp->hb_body);
    word_no = HDR_WORDS;
    if (sz > MAXOBJSZ) {
	plim = p;
    } else {
    	plim = (word *)((((word)hbp) + HBLKSIZE) - WORDS_TO_BYTES(sz));
    }
    /* go through all words in block */
	while( p <= plim ) {
	    if( mark_bit_from_hdr(hhdr, word_no)
	        && GC_has_debug_info((ptr_t)p)) {
	        ptr_t clobbered = GC_check_annotated_obj((oh *)p);
	        
	        if (clobbered != 0) {
	            GC_err_printf0(
	                "GC_check_heap_block: found smashed location at ");
        	    GC_print_smashed_obj((ptr_t)p, clobbered);
	        }
	    }
	    word_no += sz;
	    p += sz;
	}
}


/* This assumes that all accessible objects are marked, and that	*/
/* I hold the allocation lock.	Normally called by collector.		*/
void GC_check_heap_proc()
{
#   ifndef SMALL_CONFIG
	if (sizeof(oh) & (2 * sizeof(word) - 1) != 0) {
	    ABORT("Alignment problem: object header has inappropriate size\n");
	}
#   endif
    GC_apply_to_all_blocks(GC_check_heap_block, (word)0);
}

struct closure {
    GC_finalization_proc cl_fn;
    GC_PTR cl_data;
};

# ifdef __STDC__
    void * GC_make_closure(GC_finalization_proc fn, void * data)
# else
    GC_PTR GC_make_closure(fn, data)
    GC_finalization_proc fn;
    GC_PTR data;
# endif
{
    struct closure * result =
    		(struct closure *) GC_malloc(sizeof (struct closure));
    
    result -> cl_fn = fn;
    result -> cl_data = data;
    return((GC_PTR)result);
}

# ifdef __STDC__
    void GC_debug_invoke_finalizer(void * obj, void * data)
# else
    void GC_debug_invoke_finalizer(obj, data)
    char * obj;
    char * data;
# endif
{
    register struct closure * cl = (struct closure *) data;
    
    (*(cl -> cl_fn))((GC_PTR)((char *)obj + sizeof(oh)), cl -> cl_data);
} 


# ifdef __STDC__
    void GC_debug_register_finalizer(GC_PTR obj, GC_finalization_proc fn,
    				     GC_PTR cd, GC_finalization_proc *ofn,
				     GC_PTR *ocd)
# else
    void GC_debug_register_finalizer(obj, fn, cd, ofn, ocd)
    GC_PTR obj;
    GC_finalization_proc fn;
    GC_PTR cd;
    GC_finalization_proc *ofn;
    GC_PTR *ocd;
# endif
{
    ptr_t base = GC_base(obj);
    if (0 == base || (ptr_t)obj - base != sizeof(oh)) {
        GC_err_printf1(
	    "GC_register_finalizer called with non-base-pointer 0x%lx\n",
	    obj);
    }
    GC_register_finalizer(base, GC_debug_invoke_finalizer,
    			  GC_make_closure(fn,cd), ofn, ocd);
}

# ifdef __STDC__
    void GC_debug_register_finalizer_no_order
    				    (GC_PTR obj, GC_finalization_proc fn,
    				     GC_PTR cd, GC_finalization_proc *ofn,
				     GC_PTR *ocd)
# else
    void GC_debug_register_finalizer_no_order
    				    (obj, fn, cd, ofn, ocd)
    GC_PTR obj;
    GC_finalization_proc fn;
    GC_PTR cd;
    GC_finalization_proc *ofn;
    GC_PTR *ocd;
# endif
{
    ptr_t base = GC_base(obj);
    if (0 == base || (ptr_t)obj - base != sizeof(oh)) {
        GC_err_printf1(
	  "GC_register_finalizer_no_order called with non-base-pointer 0x%lx\n",
	  obj);
    }
    GC_register_finalizer_no_order(base, GC_debug_invoke_finalizer,
     			  	      GC_make_closure(fn,cd), ofn, ocd);
 }

# ifdef __STDC__
    void GC_debug_register_finalizer_ignore_self
    				    (GC_PTR obj, GC_finalization_proc fn,
    				     GC_PTR cd, GC_finalization_proc *ofn,
				     GC_PTR *ocd)
# else
    void GC_debug_register_finalizer_ignore_self
    				    (obj, fn, cd, ofn, ocd)
    GC_PTR obj;
    GC_finalization_proc fn;
    GC_PTR cd;
    GC_finalization_proc *ofn;
    GC_PTR *ocd;
# endif
{
    ptr_t base = GC_base(obj);
    if (0 == base || (ptr_t)obj - base != sizeof(oh)) {
        GC_err_printf1(
	    "GC_register_finalizer_ignore_self called with non-base-pointer 0x%lx\n",
	    obj);
    }
    GC_register_finalizer_ignore_self(base, GC_debug_invoke_finalizer,
    			  	      GC_make_closure(fn,cd), ofn, ocd);
}
