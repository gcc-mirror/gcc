/* 
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991, 1992 by Xerox Corporation.  All rights reserved.
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
/* Boehm, October 9, 1995 1:09 pm PDT */
  
/* Routines for maintaining maps describing heap block
 * layouts for various object sizes.  Allows fast pointer validity checks
 * and fast location of object start locations on machines (such as SPARC)
 * with slow division.
 */
 
# include "gc_priv.h"

char * GC_invalid_map = 0;

/* Invalidate the object map associated with a block.	Free blocks	*/
/* are identified by invalid maps.					*/
void GC_invalidate_map(hhdr)
hdr *hhdr;
{
    register int displ;
    
    if (GC_invalid_map == 0) {
        GC_invalid_map = GC_scratch_alloc(MAP_SIZE);
        if (GC_invalid_map == 0) {
            GC_err_printf0(
            	"Cant initialize GC_invalid_map: insufficient memory\n");
            EXIT();
        }
        for (displ = 0; displ < HBLKSIZE; displ++) {
            MAP_ENTRY(GC_invalid_map, displ) = OBJ_INVALID;
        }
    }
    hhdr -> hb_map = GC_invalid_map;
}

/* Consider pointers that are offset bytes displaced from the beginning */
/* of an object to be valid.                                            */

# if defined(__STDC__) || defined(__cplusplus)
    void GC_register_displacement(GC_word offset)
# else
    void GC_register_displacement(offset) 
    GC_word offset;
# endif
{
# ifndef ALL_INTERIOR_POINTERS
    DCL_LOCK_STATE;
    
    DISABLE_SIGNALS();
    LOCK();
    GC_register_displacement_inner(offset);
    UNLOCK();
    ENABLE_SIGNALS();
# endif
}

void GC_register_displacement_inner(offset) 
word offset;
{
# ifndef ALL_INTERIOR_POINTERS
    register unsigned i;
    
    if (offset > MAX_OFFSET) {
        ABORT("Bad argument to GC_register_displacement");
    }
    if (!GC_valid_offsets[offset]) {
      GC_valid_offsets[offset] = TRUE;
      GC_modws_valid_offsets[offset % sizeof(word)] = TRUE;
      for (i = 0; i <= MAXOBJSZ; i++) {
          if (GC_obj_map[i] != 0) {
             if (i == 0) {
               GC_obj_map[i][offset + HDR_BYTES] = (char)BYTES_TO_WORDS(offset);
             } else {
               register unsigned j;
               register unsigned lb = WORDS_TO_BYTES(i);
               
               if (offset < lb) {
                 for (j = offset + HDR_BYTES; j < HBLKSIZE; j += lb) {
                   GC_obj_map[i][j] = (char)BYTES_TO_WORDS(offset);
                 }
               }
             }
          }
      }
    }
# endif
}


/* Add a heap block map for objects of size sz to obj_map.	*/
/* Return FALSE on failure.					*/
GC_bool GC_add_map_entry(sz)
word sz;
{
    register unsigned obj_start;
    register unsigned displ;
    register char * new_map;
    
    if (sz > MAXOBJSZ) sz = 0;
    if (GC_obj_map[sz] != 0) {
        return(TRUE);
    }
    new_map = GC_scratch_alloc(MAP_SIZE);
    if (new_map == 0) return(FALSE);
#   ifdef PRINTSTATS
        GC_printf1("Adding block map for size %lu\n", (unsigned long)sz);
#   endif
    for (displ = 0; displ < HBLKSIZE; displ++) {
        MAP_ENTRY(new_map,displ) = OBJ_INVALID;
    }
    if (sz == 0) {
        for(displ = 0; displ <= MAX_OFFSET; displ++) {
            if (OFFSET_VALID(displ)) {
                MAP_ENTRY(new_map,displ+HDR_BYTES) = BYTES_TO_WORDS(displ);
            }
        }
    } else {
        for (obj_start = HDR_BYTES;
             obj_start + WORDS_TO_BYTES(sz) <= HBLKSIZE;
             obj_start += WORDS_TO_BYTES(sz)) {
             for (displ = 0; displ < WORDS_TO_BYTES(sz); displ++) {
                 if (OFFSET_VALID(displ)) {
                     MAP_ENTRY(new_map, obj_start + displ) =
                     				BYTES_TO_WORDS(displ);
                 }
             }
        }
    }
    GC_obj_map[sz] = new_map;
    return(TRUE);
}
