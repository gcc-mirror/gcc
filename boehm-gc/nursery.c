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

#ifdef NURSERY
??? This implementation is incomplete.  If you are trying to
??? compile this you are doing something wrong.

#include "nursery.h"

#define SCAN_STATICS_FOR_NURSERY
	/* If this is not defined, the collector will not see 	*/
	/* references from static data areas to the nursery.	*/

struct copy_obj {
    ptr_t forward;	/* Forwarding link for copied objects.	*/
    GC_copy_descriptor descr; /* Object descriptor	*/
    word data[1];
}

ptr_t GC_nursery_start;	/* Start of nursery area.	*/
			/* Must be NURSERY_BLOCK_SIZE	*/
			/* aligned.			*/
ptr_t GC_nursery_end;	/* End of nursery area.		*/
unsigned char * GC_nursery_map;
			/* GC_nursery_map[i] != 0 if an object	*/
			/* starts on the ith 64-bit "word" of 	*/
			/* nursery.  This simple structure has	*/
			/* the advantage that 			*/
			/* allocation is cheap.  Lookup is 	*/
			/* cheap for pointers to the head of	*/
			/* an object, which should be the	*/
			/* usual case.				*/
#   define NURSERY_MAP_NOT_START	0  /* Not start of object. */
#   define NURSERY_MAP_START		1  /* Start of object.	   */
#   define NURSERY_MAP_PINNED		2  /* Start of pinned obj. */

# ifdef ALIGN_DOUBLE
#   define NURSERY_WORD_SIZE (2 * sizeof(word))
# else
#   define NURSERY_WORD_SIZE sizeof(word)
# endif

# define NURSERY_BLOCK_SIZE (HBLKSIZE/2)	
	/* HBLKSIZE must be a multiple of NURSERY_BLOCK_SIZE */
# define NURSERY_SIZE (1024 * NURSERY_BLOCK_SIZE)

size_t GC_nursery_size = NURSERY_SIZE;
			/* Must be multiple of NURSERY_BLOCK_SIZE	*/

size_t GC_nursery_blocks; /* Number of blocks in the nursery.	*/

unsigned GC_next_nursery_block; /* index of next block we will attempt 	*/
				/* allocate from during this cycle.	*/
				/* If it is pinned, we won't actually	*/
				/* use it.				*/

unsigned short *GC_pinned;	/* Number of pinned objects in ith	*/
				/* nursery block.			*/
				/* GC_pinned[i] != 0 if the ith nursery */
				/* block is pinned, and thus not used	*/
				/* for allocation.			*/

GC_copy_alloc_state global_alloc_state = (ptr_t)(-1);	/* will overflow. */

/* Array of known rescuing pointers from the heap to the nursery.	*/
  ptr_t ** nursery_rescuers;
  /* Pointer to one past the last slot in rescuer table	*/
  ptr_t ** nursery_rescuers_end;
  /* Maximum number of known rescuing pointers.			*/
# define MAX_NURSERY_RESCUERS 32*1024
  /* Add a rescuer to the list	*/
# define ADD_NURSERY_RESCUER(p) \
    if (nursery_rescuers_end >= nursery_rescuers + MAX_NURSERY_RESCUERS) { \
      ABORT("Nursery recuers overflow"); /* Fix later !!! */ \
    } else { \
      *nursery_rescuers_end++ = p; \
    }
  /* Remove rescuer at the given position in the table	*/
# define REMOVE_RESCUER(p) \
    *p = *--nursery_rescuers_end

/* Should be called with allocator lock held.	*/
GC_nursery_init() {
    GC_nursery_start = GET_MEM(GC_nursery_size);
    GC_nursery_end = GC_nursery_start + GC_nursery_size;
    GC_next_nursery_block = 0;
    if (GC_nursery_start < GC_least_plausible_heap_addr) { 
        GC_least_plausible_heap_addr = GC_nursery_start;   
    }
    if (GC_nursery_end > GC_greatest_plausible_heap_addr) {
        GC_greatest_plausible_heap_addr = GC_nursery_end;  
    }
    if (GC_nursery_start & (NURSERY_BLOCK_SIZE-1)) {
	GC_err_printf1("Nursery area is misaligned!!");
	/* This should be impossible, since GET_MEM returns HBLKSIZE */
	/* aligned chunks, and that should be a multiple of 	     */
	/* NURSERY_BLOCK_SIZE					     */
	ABORT("misaligned nursery");
    }
    GC_nursery_map = GET_MEM(GC_nursery_size/NURSERY_WORD_SIZE);
    /* Map is cleared a block at a time when we allocate from the block. */
    /* BZERO(GC_nursery_map, GC_nursery_size/NURSERY_WORD_SIZE);	 */
    GC_nursery_blocks = GC_nursery_size/NURSERY_BLOCK_SIZE;
    GC_pinned = GC_scratch_alloc(GC_nursery_blocks * sizeof(unsigned short));
    BZERO(GC_pinned, GC_nursery_blocks);
    nursery_rescuers = GET_MEM(MAX_NURSERY_RESCUERS * sizeof(ptr_t *));
    nursery_rescuers_end = nursery_rescuers;
    if (0 == GC_nursery_start || 0 == GC_nursery_map || 0 == nursery_rescuers)
	ABORT("Insufficient memory for nursery");
}

#define PIN_OBJ(p) \
    if (p >= GC_nursery_start && p < GC_nursery_end) { GC_pin_obj_checked(p); }

/* Pin the object at p, if it's in the nursery.	*/
void GC_pin_obj(ptr_t p) {
    PIN_OBJ(p);
}

void (*GC_push_proc)(ptr_t) = 0;

/* Pin the object at p, which is known to be in the nursery.	*/
void GC_pin_obj_checked(ptr_t p) {
    unsigned offset = p - GC_nursery_start;
    unsigned word_offset = BYTES_TO_WORDS(offset);
    unsigned blockno = (current - GC_nursery_start)/NURSERY_BLOCK_SIZE;
    while (GC_nursery_map[word_offset] == NURSERY_MAP_NOT_START) {
	--word_offset;    
    }
    if (GC_nursery_map[word_offset] != NURSERY_MAP_PINNED) {
        GC_nursery_map[word_offset] = NURSERY_MAP_PINNED;
        ++GC_pinned[blockno];
        ??Push object at GC_nursery_start + WORDS_TO_BYTES(word_offset)
        ??onto mark stack. 
    }
}

void GC_scan_region_for_nursery(ptr_t low, ptr_t high) {
#   if CPP_WORDSZ/8 != ALIGNMENT
      --> fix this
#   endif
    word * l = (word *)((word)low + ALIGNMENT - 1 & ~(ALIGNMENT - 1));
    word * h = (word *)((word)high & ~(ALIGNMENT - 1));
    word * p;
    for (p = l; p < h; ++p) {
	PIN_OBJ(p);
    }
}

/* Invoke GC_scan_region_for_nursery on ranges that are not excluded. */
void GC_scan_region_for_nursery_with_exclusions(ptr_t bottom, ptr_t top)
{
    struct exclusion * next;
    ptr_t excl_start;

    while (bottom < top) {
        next = GC_next_exclusion(bottom);
	if (0 == next || (excl_start = next -> e_start) >= top) {
	    GC_scan_region_for_nursery(bottom, top);
	    return;
	}
	if (excl_start > bottom)
		GC_scan_region_for_nursery(bottom, excl_start);
	bottom = next -> e_end;
    }
}


void GC_scan_stacks_for_nursery(void) {
#   ifdef THREADS
	--> fix this
#   endif
#   ifdef STACK_GROWS_DOWN
      ptr_t stack_low = GC_approx_sp();
      ptr_t stack_high = GC_stackbottom;
#   else
      ptr_t stack_low = GC_stackbottom;
      ptr_t stack_high = GC_approx_sp();
#   endif
    GC_scan_region_for_nursery(stack_low, stack_high);
#   ifdef IA64
      GC_scan_region_for_nursery(BACKING_STORE_BASE,
				 (ptr_t) GC_save_regs_ret_val);
#   endif
}

void GC_scan_roots_for_nursery(void) {
  /* Scan registers.	*/
    /* Direct GC_push_one to call GC_pin_obj instead of marking	*/
    /* and pushing objects.					*/
    /* This is a bit ugly, but we don't have to touch the	*/
    /* platform-dependent code.					*/
     
    void (*old_push_proc)(ptr_t) = GC_push_proc;
    GC_push_proc = GC_pin_obj;
    GC_push_regs();
    GC_push_proc = old_push_proc;
  GC_scan_stacks_for_nursery();
# ifdef SCAN_STATICS_FOR_NURSERY
#   if (defined(DYNAMIC_LOADING) || defined(MSWIN32) || defined(PCR)) \
        && !defined(SRC_M3)
      GC_remove_tmp_roots();
      GC_register_dynamic_libraries();
#   endif
    /* Mark everything in static data areas                             */
      for (i = 0; i < n_root_sets; i++) {
        GC_scan_region_for_nursery_with_exclusions (
			     GC_static_roots[i].r_start,
			     GC_static_roots[i].r_end);
     }
# endif
}

/* Array of known rescuing pointers from the heap to the nursery.	*/
ptr_t ** nursery_rescuers;

/* Caller holds allocation lock.	*/
void GC_collect_nursery(void) {
    int i;
    ptr_t scan_ptr = 0;
    STOP_WORLD;
    for (i = 0; i < GC_nursery_blocks; ++i) GC_pinned[i] = 0;
    GC_scan_roots_for_nursery();
    /* All objects referenced by roots are now pinned. 		*/
    /* Their contents are described by 			 	*/
    /* mark stack entries.					*/

    /* Pin blocks corresponding to valid allocation states.	*/
    /* that probably happens automagically if the allocation	*/
    /* states are kept where we can see them.			*/
    /* It will take work if static roots are not scanned.	*/
    /* We want to do this both for correctness and to avoid	*/
    /* promoting very young objects.				*/

    /* Somehow capture dirty bits.  Update rescuers array to	*/
    /* reflect newly valid and invalid references from dirty 	*/
    /* pages.  Other references should remain valid, since the	*/
    /* referents should have been pinned.			*/

    /* Traverse the old object heap.  Pin objects in the 	*/
    /* nursery that are ambiguously referenced, copy those	*/
    /* that are unambiguously referenced.			*/

    /* Traverse objects in mark stack.				*/
    /* If referenced object is in pinned block, add contents	*/
    /* to mark stack.  If referenced object is forwarded,	*/
    /* update pointer.  Otherwise reallocate the object	in the	*/
    /* old heap, copy its contents, and then enqueue its 	*/
    /* contents in the mark stack.				*/
    START_WORLD;
}

/* Initialize an allocation state so that it can be used for 	*/
/* allocation.  This implicitly reserves a small section of the	*/
/* nursery for use with this allocator.				*/
/* Also called to replenish an allocator that has been 		*/
/* exhausted.							*/
void GC_init_copy_alloc_state(GC_copy_alloc_state *)
    unsigned next_block;
    ptr_t block_addr;
    LOCK();
    next_block = GC_next_nursery_block;
    while(is_pinned[next_block] && next_block < GC_nursery_blocks) {
	++next_block;
    }
    if (next_block < GC_nursery_blocks) {
	block_addr = GC_nursery_start + NURSERY_BLOCK_SIZE * next_block;
   	GC_next_nursery_block = next_block + 1;
	BZERO(GC_nursery_map + next_block *
				(NURSERY_BLOCK_SIZE/NURSERY_WORD_SIZE),
	      NURSERY_BLOCK_SIZE/NURSERY_WORD_SIZE);
	*GC_copy_alloc_state = block_addr;
	UNLOCK();
    } else {
     	GC_collect_nursery();
    	GC_next_nursery_block = 0;
    	UNLOCK();
    	get_new_block(s);
    }
}

GC_PTR GC_copying_malloc2(GC_copy_descriptor *d, GC_copy_alloc_state *s) {
    size_t sz = GC_SIZE_FROM_DESCRIPTOR(d);
    ptrdiff_t offset;
    ptr_t result = *s;
    ptr_t new = result + sz;
    if (new & COPY_BLOCK_MASK <= result & COPY_BLOCK_MASK> {
	GC_init_copy_alloc_state(s);
	result = *s;
	new = result + sz;
        GC_ASSERT(new & COPY_BLOCK_MASK > result & COPY_BLOCK_MASK>
    }
    (struct copy_obj *)result -> descr = d;      
    (struct copy_obj *)result -> forward = 0;      
    offset = (result - GC_nursery_start)/NURSERY_WORD_SIZE;
    GC_nursery_map[offset] = NURSERY_MAP_NOT_START;
}

GC_PTR GC_copying_malloc(GC_copy_descriptor *d) {
}

#endif /* NURSERY */
