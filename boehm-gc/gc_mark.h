/*
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
 *
 */
/* Boehm, November 7, 1994 4:56 pm PST */

/*
 * Declarations of mark stack.  Needed by marker and client supplied mark
 * routines.  To be included after gc_priv.h.
 */
#ifndef GC_MARK_H
# define GC_MARK_H

/* A client supplied mark procedure.  Returns new mark stack pointer.	*/
/* Primary effect should be to push new entries on the mark stack.	*/
/* Mark stack pointer values are passed and returned explicitly.	*/
/* Global variables decribing mark stack are not necessarily valid.	*/
/* (This usually saves a few cycles by keeping things in registers.)	*/
/* Assumed to scan about PROC_BYTES on average.  If it needs to do	*/
/* much more work than that, it should do it in smaller pieces by	*/
/* pushing itself back on the mark stack.				*/
/* Note that it should always do some work (defined as marking some	*/
/* objects) before pushing more than one entry on the mark stack.	*/
/* This is required to ensure termination in the event of mark stack	*/
/* overflows.								*/
/* This procedure is always called with at least one empty entry on the */
/* mark stack.								*/
/* Currently we require that mark procedures look for pointers in a	*/
/* subset of the places the conservative marker would.  It must be safe	*/
/* to invoke the normal mark procedure instead.				*/
# define PROC_BYTES 100
/* The real declarations of the following are in gc_priv.h, so that	*/
/* we can avoid scanning the following table.				*/
/*
typedef struct ms_entry * (*mark_proc)(   word * addr, mark_stack_ptr,
					  mark_stack_limit, env   );
					  
# define LOG_MAX_MARK_PROCS 6
# define MAX_MARK_PROCS (1 << LOG_MAX_MARK_PROCS)
extern mark_proc GC_mark_procs[MAX_MARK_PROCS];
*/

extern word GC_n_mark_procs;

/* Object descriptors on mark stack or in objects.  Low order two	*/
/* bits are tags distinguishing among the following 4 possibilities	*/
/* for the high order 30 bits.						*/
#define DS_TAG_BITS 2
#define DS_TAGS   ((1 << DS_TAG_BITS) - 1)
#define DS_LENGTH 0	/* The entire word is a length in bytes that	*/
			/* must be a multiple of 4.			*/
#define DS_BITMAP 1	/* 30 bits are a bitmap describing pointer	*/
			/* fields.  The msb is 1 iff the first word	*/
			/* is a pointer.				*/
			/* (This unconventional ordering sometimes	*/
			/* makes the marker slightly faster.)		*/
			/* Zeroes indicate definite nonpointers.  Ones	*/
			/* indicate possible pointers.			*/
			/* Only usable if pointers are word aligned.	*/
#   define BITMAP_BITS (WORDSZ - DS_TAG_BITS)
#define DS_PROC   2
			/* The objects referenced by this object can be */
			/* pushed on the mark stack by invoking		*/
			/* PROC(descr).  ENV(descr) is passed as the	*/
			/* last argument.				*/
#   define PROC(descr) \
		(GC_mark_procs[((descr) >> DS_TAG_BITS) & (MAX_MARK_PROCS-1)])
#   define ENV(descr) \
		((descr) >> (DS_TAG_BITS + LOG_MAX_MARK_PROCS))
#   define MAX_ENV \
  	      (((word)1 << (WORDSZ - DS_TAG_BITS - LOG_MAX_MARK_PROCS)) - 1)
#   define MAKE_PROC(proc_index, env) \
	    (((((env) << LOG_MAX_MARK_PROCS) | (proc_index)) << DS_TAG_BITS) \
	    | DS_PROC)
#define DS_PER_OBJECT 3	/* The real descriptor is at the		*/
			/* byte displacement from the beginning of the	*/
			/* object given by descr & ~DS_TAGS		*/
			
typedef struct ms_entry {
    word * mse_start;   /* First word of object */
    word mse_descr;	/* Descriptor; low order two bits are tags,	*/
    			/* identifying the upper 30 bits as one of the	*/
    			/* following:					*/
} mse;

extern word GC_mark_stack_size;

extern mse * GC_mark_stack_top;

extern mse * GC_mark_stack;

word GC_find_start();

mse * GC_signal_mark_stack_overflow();

# ifdef GATHERSTATS
#   define ADD_TO_ATOMIC(sz) GC_atomic_in_use += (sz)
#   define ADD_TO_COMPOSITE(sz) GC_composite_in_use += (sz)
# else
#   define ADD_TO_ATOMIC(sz)
#   define ADD_TO_COMPOSITE(sz)
# endif

/* Push the object obj with corresponding heap block header hhdr onto 	*/
/* the mark stack.							*/
# define PUSH_OBJ(obj, hhdr, mark_stack_top, mark_stack_limit) \
{ \
    register word _descr = (hhdr) -> hb_descr; \
        \
    if (_descr == 0) { \
    	ADD_TO_ATOMIC((hhdr) -> hb_sz); \
    } else { \
        ADD_TO_COMPOSITE((hhdr) -> hb_sz); \
        mark_stack_top++; \
        if (mark_stack_top >= mark_stack_limit) { \
          mark_stack_top = GC_signal_mark_stack_overflow(mark_stack_top); \
        } \
        mark_stack_top -> mse_start = (obj); \
        mark_stack_top -> mse_descr = _descr; \
    } \
}

#ifdef PRINT_BLACK_LIST
#   define GC_FIND_START(current, hhdr, source) \
	GC_find_start(current, hhdr, source)
#else
#   define GC_FIND_START(current, hhdr, source) \
	GC_find_start(current, hhdr)
#endif

/* Push the contents of current onto the mark stack if it is a valid	*/
/* ptr to a currently unmarked object.  Mark it.			*/
/* If we assumed a standard-conforming compiler, we could probably	*/
/* generate the exit_label transparently.				*/
# define PUSH_CONTENTS(current, mark_stack_top, mark_stack_limit, \
		       source, exit_label) \
{ \
    register int displ;  /* Displacement in block; first bytes, then words */ \
    register hdr * hhdr; \
    register map_entry_type map_entry; \
    \
    GET_HDR(current,hhdr); \
    if (IS_FORWARDING_ADDR_OR_NIL(hhdr)) { \
         current = GC_FIND_START(current, hhdr, (word)source); \
         if (current == 0) goto exit_label; \
         hhdr = HDR(current); \
    } \
    displ = HBLKDISPL(current); \
    map_entry = MAP_ENTRY((hhdr -> hb_map), displ); \
    if (map_entry == OBJ_INVALID) { \
        GC_ADD_TO_BLACK_LIST_NORMAL(current, source); goto exit_label; \
    } \
    displ = BYTES_TO_WORDS(displ); \
    displ -= map_entry; \
	\
    { \
        register word * mark_word_addr = hhdr -> hb_marks + divWORDSZ(displ); \
        register word mark_word = *mark_word_addr; \
        register word mark_bit = (word)1 << modWORDSZ(displ); \
          \
        if (mark_word & mark_bit) { \
	      /* Mark bit is already set */ \
	      goto exit_label; \
        } \
        GC_STORE_BACK_PTR((ptr_t)source, (ptr_t)HBLKPTR(current) \
				      + WORDS_TO_BYTES(displ)); \
        *mark_word_addr = mark_word | mark_bit; \
    } \
    PUSH_OBJ(((word *)(HBLKPTR(current)) + displ), hhdr, \
    	     mark_stack_top, mark_stack_limit) \
  exit_label: ; \
}

#ifdef PRINT_BLACK_LIST
#   define PUSH_ONE_CHECKED(p, ip, source) \
	GC_push_one_checked(p, ip, (ptr_t)(source))
#else
#   define PUSH_ONE_CHECKED(p, ip, source) \
	GC_push_one_checked(p, ip)
#endif

/*
 * Push a single value onto mark stack. Mark from the object pointed to by p.
 * P is considered valid even if it is an interior pointer.
 * Previously marked objects are not pushed.  Hence we make progress even
 * if the mark stack overflows.
 */
# define GC_PUSH_ONE_STACK(p, source) \
    if ((ptr_t)(p) >= GC_least_plausible_heap_addr 	\
	 && (ptr_t)(p) < GC_greatest_plausible_heap_addr) {	\
	 PUSH_ONE_CHECKED(p, TRUE, source);	\
    }

/*
 * As above, but interior pointer recognition as for
 * normal for heap pointers.
 */
# ifdef ALL_INTERIOR_POINTERS
#   define AIP TRUE
# else
#   define AIP FALSE
# endif
# define GC_PUSH_ONE_HEAP(p,source) \
    if ((ptr_t)(p) >= GC_least_plausible_heap_addr 	\
	 && (ptr_t)(p) < GC_greatest_plausible_heap_addr) {	\
	 PUSH_ONE_CHECKED(p,AIP,source);	\
    }

/*
 * Mark from one finalizable object using the specified
 * mark proc. May not mark the object pointed to by 
 * real_ptr. That is the job of the caller, if appropriate
 */
# define GC_MARK_FO(real_ptr, mark_proc) \
{ \
    (*(mark_proc))(real_ptr); \
    while (!GC_mark_stack_empty()) GC_mark_from_mark_stack(); \
    if (GC_mark_state != MS_NONE) { \
        GC_set_mark_bit(real_ptr); \
        while (!GC_mark_some((ptr_t)0)); \
    } \
}

extern GC_bool GC_mark_stack_too_small;
				/* We need a larger mark stack.  May be	*/
				/* set by client supplied mark routines.*/

typedef int mark_state_t;	/* Current state of marking, as follows:*/
				/* Used to remember where we are during */
				/* concurrent marking.			*/

				/* We say something is dirty if it was	*/
				/* written since the last time we	*/
				/* retrieved dirty bits.  We say it's 	*/
				/* grungy if it was marked dirty in the	*/
				/* last set of bits we retrieved.	*/
				
				/* Invariant I: all roots and marked	*/
				/* objects p are either dirty, or point */
				/* to objects q that are either marked 	*/
				/* or a pointer to q appears in a range	*/
				/* on the mark stack.			*/

# define MS_NONE 0		/* No marking in progress. I holds.	*/
				/* Mark stack is empty.			*/

# define MS_PUSH_RESCUERS 1	/* Rescuing objects are currently 	*/
				/* being pushed.  I holds, except	*/
				/* that grungy roots may point to 	*/
				/* unmarked objects, as may marked	*/
				/* grungy objects above scan_ptr.	*/

# define MS_PUSH_UNCOLLECTABLE 2
				/* I holds, except that marked 		*/
				/* uncollectable objects above scan_ptr */
				/* may point to unmarked objects.	*/
				/* Roots may point to unmarked objects	*/

# define MS_ROOTS_PUSHED 3	/* I holds, mark stack may be nonempty  */

# define MS_PARTIALLY_INVALID 4	/* I may not hold, e.g. because of M.S. */
				/* overflow.  However marked heap	*/
				/* objects below scan_ptr point to	*/
				/* marked or stacked objects.		*/

# define MS_INVALID 5		/* I may not hold.			*/

extern mark_state_t GC_mark_state;

#endif  /* GC_MARK_H */

