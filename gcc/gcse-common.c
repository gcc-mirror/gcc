/* Shared code for before and after reload gcse implementations.
   Copyright (C) 1997-2017 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>. 

   It is expected that more hunks of gcse.c and postreload-gcse.c should
   migrate into this file.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "gcse-common.h"


/* Record all of the canonicalized MEMs of record_last_mem_set_info's insn.
   Note we store a pair of elements in the list, so they have to be
   taken off pairwise.  */

void
canon_list_insert (rtx dest, const_rtx x ATTRIBUTE_UNUSED, void *data)
{
  rtx dest_addr;
  int bb;
  modify_pair pair;

  while (GET_CODE (dest) == SUBREG
      || GET_CODE (dest) == ZERO_EXTRACT
      || GET_CODE (dest) == STRICT_LOW_PART)
    dest = XEXP (dest, 0);

  /* If DEST is not a MEM, then it will not conflict with a load.  Note
     that function calls are assumed to clobber memory, but are handled
     elsewhere.  */

  if (! MEM_P (dest))
    return;

  dest_addr = get_addr (XEXP (dest, 0));
  dest_addr = canon_rtx (dest_addr);
  rtx_insn *insn = ((struct gcse_note_stores_info *)data)->insn;
  bb = BLOCK_FOR_INSN (insn)->index;

  pair.dest = dest;
  pair.dest_addr = dest_addr;
  vec<modify_pair> *canon_mem_list
    = ((struct gcse_note_stores_info *)data)->canon_mem_list;
  canon_mem_list[bb].safe_push (pair);
}

/* Record memory modification information for INSN.  We do not actually care
   about the memory location(s) that are set, or even how they are set (consider
   a CALL_INSN).  We merely need to record which insns modify memory.  */

void
record_last_mem_set_info_common (rtx_insn *insn,
				 vec<rtx_insn *> *modify_mem_list,
				 vec<modify_pair> *canon_modify_mem_list,
				 bitmap modify_mem_list_set,
				 bitmap blocks_with_calls)

{
  int bb;

  bb = BLOCK_FOR_INSN (insn)->index;
  modify_mem_list[bb].safe_push (insn);
  bitmap_set_bit (modify_mem_list_set, bb);

  if (CALL_P (insn))
    bitmap_set_bit (blocks_with_calls, bb);
  else
    {
      struct gcse_note_stores_info data;
      data.insn = insn;
      data.canon_mem_list = canon_modify_mem_list;
      note_stores (PATTERN (insn), canon_list_insert, (void*) &data);
    }
}


/* For each block, compute whether X is transparent.  X is either an
   expression or an assignment [though we don't care which, for this context
   an assignment is treated as an expression].  For each block where an
   element of X is modified, reset the INDX bit in BMAP. 

   BLOCKS_WITH_CALLS indicates which blocks contain CALL_INSNs which kill
   memory.

   MODIFY_MEM_LIST_SET indicates which blocks have memory stores which might
   kill a particular memory location.

   CANON_MODIFY_MEM_LIST is the canonicalized list of memory locations modified
   for each block.  */

void
compute_transp (const_rtx x, int indx, sbitmap *bmap,
		bitmap blocks_with_calls,
		bitmap modify_mem_list_set,
	        vec<modify_pair> *canon_modify_mem_list)
{
  int i, j;
  enum rtx_code code;
  const char *fmt;

  /* repeat is used to turn tail-recursion into iteration since GCC
     can't do it when there's no return value.  */
 repeat:

  if (x == 0)
    return;

  code = GET_CODE (x);
  switch (code)
    {
    case REG:
	{
	  df_ref def;
	  for (def = DF_REG_DEF_CHAIN (REGNO (x));
	       def;
	       def = DF_REF_NEXT_REG (def))
	    bitmap_clear_bit (bmap[DF_REF_BB (def)->index], indx);
	}

      return;

    case MEM:
      if (! MEM_READONLY_P (x))
	{
	  bitmap_iterator bi;
	  unsigned bb_index;
	  rtx x_addr;

	  x_addr = get_addr (XEXP (x, 0));
	  x_addr = canon_rtx (x_addr);

	  /* First handle all the blocks with calls.  We don't need to
	     do any list walking for them.  */
	  EXECUTE_IF_SET_IN_BITMAP (blocks_with_calls, 0, bb_index, bi)
	    {
	      bitmap_clear_bit (bmap[bb_index], indx);
	    }

	  /* Now iterate over the blocks which have memory modifications
	     but which do not have any calls.  */
	  EXECUTE_IF_AND_COMPL_IN_BITMAP (modify_mem_list_set,
					  blocks_with_calls,
					  0, bb_index, bi)
	    {
	      vec<modify_pair> list
		= canon_modify_mem_list[bb_index];
	      modify_pair *pair;
	      unsigned ix;

	      FOR_EACH_VEC_ELT_REVERSE (list, ix, pair)
		{
		  rtx dest = pair->dest;
		  rtx dest_addr = pair->dest_addr;

		  if (canon_true_dependence (dest, GET_MODE (dest),
					     dest_addr, x, x_addr))
		    {
		      bitmap_clear_bit (bmap[bb_index], indx);
		      break;
		    }
	        }
	    }
	}

      x = XEXP (x, 0);
      goto repeat;

    case PC:
    case CC0: /*FIXME*/
    case CONST:
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case LABEL_REF:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return;

    default:
      break;
    }

  for (i = GET_RTX_LENGTH (code) - 1, fmt = GET_RTX_FORMAT (code); i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  /* If we are about to do the last recursive call
	     needed at this level, change it into iteration.
	     This function is called enough to be worth it.  */
	  if (i == 0)
	    {
	      x = XEXP (x, i);
	      goto repeat;
	    }

	  compute_transp (XEXP (x, i), indx, bmap, blocks_with_calls,
			  modify_mem_list_set, canon_modify_mem_list);
	}
      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  compute_transp (XVECEXP (x, i, j), indx, bmap, blocks_with_calls,
			  modify_mem_list_set, canon_modify_mem_list);
    }
}
