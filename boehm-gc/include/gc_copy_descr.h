
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
/* Descriptor for allocation request. May be redefined by client. */
typedef struct {
    GC_word bitmap;	/* Bitmap describing pointer locations.	*/
			/* High order bit correspond to 0th	*/
			/* word.  2 lsbs must be 0.		*/
    size_t length;      /* In bytes, must be multiple of word	*/
			/* size.  Must be >0, <= 512		*/
} * GC_copy_descriptor;

/* The collector accesses descriptors only through these two macros. */
#define GC_SIZE_FROM_DESCRIPTOR(d) ((d) -> length)
#define GC_BIT_MAP_FROM_DESCRIPTOR(d) ((d) -> bitmap)

