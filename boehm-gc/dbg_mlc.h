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

/*
 * This is mostly an internal header file.  Typical clients should
 * not use it.  Clients that define their own object kinds with
 * debugging allocators will probably want to include this, however.
 * No attempt is made to keep the namespace clean.  This should not be
 * included from header filrd that are frequently included by clients.
 */

#ifndef _DBG_MLC_H

#define _DBG_MLC_H

# define I_HIDE_POINTERS
# include "gc_priv.h"
# ifdef KEEP_BACK_PTRS
#   include "backptr.h"
# endif

# define START_FLAG ((word)0xfedcedcb)
# define END_FLAG ((word)0xbcdecdef)
	/* Stored both one past the end of user object, and one before	*/
	/* the end of the object as seen by the allocator.		*/


/* Object header */
typedef struct {
#   ifdef KEEP_BACK_PTRS
	ptr_t oh_back_ptr;
#	define MARKED_FOR_FINALIZATION (ptr_t)(-1)
	    /* Object was marked because it is finalizable.	*/
#	define MARKED_FROM_REGISTER (ptr_t)(-2)
	    /* Object was marked from a rgister.  Hence the	*/
	    /* source of the reference doesn't have an address.	*/
#	ifdef ALIGN_DOUBLE
	  word oh_dummy;
#	endif
#   endif
    char * oh_string;		/* object descriptor string	*/
    word oh_int;		/* object descriptor integers	*/
#   ifdef NEED_CALLINFO
      struct callinfo oh_ci[NFRAMES];
#   endif
    word oh_sz;			/* Original malloc arg.		*/
    word oh_sf;			/* start flag */
} oh;
/* The size of the above structure is assumed not to dealign things,	*/
/* and to be a multiple of the word length.				*/

#define DEBUG_BYTES (sizeof (oh) + sizeof (word))
#define USR_PTR_FROM_BASE(p) ((ptr_t)(p) + sizeof(oh))

/* There is no reason to ever add a byte at the end explicitly, since we */
/* already add a guard word.						 */
#undef ROUNDED_UP_WORDS
#define ROUNDED_UP_WORDS(n) BYTES_TO_WORDS((n) + WORDS_TO_BYTES(1) - 1)

#ifdef SAVE_CALL_CHAIN
#   define ADD_CALL_CHAIN(base, ra) GC_save_callers(((oh *)(base)) -> oh_ci)
#   define PRINT_CALL_CHAIN(base) GC_print_callers(((oh *)(base)) -> oh_ci)
#else
# ifdef GC_ADD_CALLER
#   define ADD_CALL_CHAIN(base, ra) ((oh *)(base)) -> oh_ci[0].ci_pc = (ra)
#   define PRINT_CALL_CHAIN(base) GC_print_callers(((oh *)(base)) -> oh_ci)
# else
#   define ADD_CALL_CHAIN(base, ra)
#   define PRINT_CALL_CHAIN(base)
# endif
#endif

# ifdef GC_ADD_CALLER
#   define OPT_RA ra,
# else
#   define OPT_RA
# endif


/* Check whether object with base pointer p has debugging info	*/ 
/* p is assumed to point to a legitimate object in our part	*/
/* of the heap.							*/
GC_bool GC_has_debug_info(/* p */);

/* Store debugging info into p.  Return displaced pointer. */
/* Assumes we don't hold allocation lock.		   */
ptr_t GC_store_debug_info(/* p, sz, string, integer */);

#endif /* _DBG_MLC_H */
