/* Scanning of rtl byte level scanning for dataflow analysis.
   Copyright (C) 2008  Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck (zadeck@naturalbridge.com).

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
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tm_p.h"
#include "df.h"
#include "output.h"
#include "dbgcnt.h"

/* The following suite of functions provides bytewise modeling of REFs
   which are struct df_ref.  START_BYTE and LAST_BYTE are returned.
   These can be used as indexes into bitmaps.  The indexes are
   normalized so that 0 is the lowest numbered byte, of the inner
   register according to the natural ordering of the machine.

   This code is designed to be used in backwards scans (which is, of
   course, the way all dataflow scanning should really be done).  It
   would require a lot of reworking of the api to make it work in a
   forwards scanning world.  */


/* Helper for df_compute_accessed_bytes.  Ref is some sort of extract.
   Return true if this effects the entire reg in REF.  Return false if
   otherwise and set START_BYTE and LAST_BYTE.  See the description of
   df_compute_accessed_bytes for a description of MM.  */ 

static bool 
df_compute_accessed_bytes_extract (df_ref ref,
				   enum df_mm mm ,
				   unsigned int *start_byte, 
				   unsigned int *last_byte)
{
  int start;
  int last;
  rtx reg = DF_REF_REG (ref);
  enum machine_mode m1;
  int m1_size;
  enum machine_mode m2;
  int m2_size;

  /* (*_extract:M1 (reg:M2 X) WIDTH POS)
     (*_extract:M1 (subreg:M1 (reg:M2 X N) WIDTH POS)
      
     This is a bitfield extraction.  The assignment clobbers/extracts
     exactly the bits named by WIDTH and POS and does not affect the
     other bits in register X.  It is also technically possible that
     the bits asked for are longer than units per word.  */
  
  int offset = DF_REF_EXTRACT_OFFSET (ref);
  int width = DF_REF_EXTRACT_WIDTH (ref);

  if (width == -1 || offset == -1)
    return true;

  m1 = DF_REF_EXTRACT_MODE (ref);
  m1_size = GET_MODE_SIZE (m1);

  gcc_assert (m1_size <= UNITS_PER_WORD);

  /* There is nothing to do if this is a pure big or small endian
     machine, but if the machine is a pastiche, we have to convert the
     bit offsets into byte offsets.  This is only possible because we
     do not care about individual bits because this conversion may
     make the bits non-contiguous.  */
  if (BYTES_BIG_ENDIAN != BITS_BIG_ENDIAN)
    offset = GET_MODE_BITSIZE (m1_size) - (offset + width);

  /* The offset is now in the same order as the subreg_byte.  */
  if (GET_CODE (reg) == SUBREG)
    {
      m2 = GET_MODE (SUBREG_REG (reg));
      m2_size = GET_MODE_SIZE (m2);
      if (m1_size > m2_size)
	/* If it is paradoxical, subreg_byte will be zero.  */
	offset -= subreg_lowpart_offset (m2, m1) * BITS_PER_UNIT;
      else
	offset += SUBREG_BYTE (reg) * BITS_PER_UNIT;
    }
  else
    {
      m2 = GET_MODE (reg);
      m2_size = GET_MODE_SIZE (m2);
    }

  if (mm == DF_MM_MUST)
    {
      /* For defs (generally), count the byte only if the whole byte
	 is touched.  */
      start = (offset + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
      last = (width + offset) / BITS_PER_UNIT;

      /* In the case where there is nothing, start may be one larger
	 than last, we canonize this to return zeros.  This keeps
	 computations of length from being negative.  */
      if (start >= last)
	{
	  start = 0;
	  last = 0;
	}
    }
  else
    {
      /* For uses (generally), count the byte if any part of the byte
	 is touched.  */
      start = offset / BITS_PER_UNIT;
      last = (width + offset + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
    }

  /* Paradoxical truncation.  */
  if (start < 0)
    start = 0;
  if (last > m2_size)
    last = m2_size;

  if (dump_file)
    fprintf (dump_file, "    cpb extract regno=%d start=%d last=%d\n", 
	     DF_REF_REGNO (ref), start, last);
  
  *start_byte = start;
  *last_byte = last;
  return false;
}


/* Helper for df_compute_accessed_bytes.  Ref is a strict_low_part.
   Return true if this effects the entire reg in REF. Return false if
   otherwise and set START_BYTE and LAST_BYTE.  */ 

static bool 
df_compute_accessed_bytes_strict_low_part (df_ref ref, 
					   unsigned int *start_byte, 
					   unsigned int *last_byte)
{
  int start;
  int last;
  rtx reg = DF_REF_REG (ref);
  enum machine_mode m1;
  int m1_size;
  enum machine_mode m2;
  int m2_size;
  int offset;

  /* In order to accommodate multiword subregs of a hardreg, df_scan
     eats the subreg and it can only be found from the loc.  */
  if (REG_P (reg))
    reg = *(DF_REF_LOC (ref));

  m1 = GET_MODE (reg);
  m1_size = GET_MODE_SIZE (m1);
  m2 = GET_MODE (SUBREG_REG (reg));
  m2_size = GET_MODE_SIZE (m2);
  offset = SUBREG_BYTE (reg);

  /* It does not seem to be meaningful to apply a strict_low_part of a
     paradoxical register.  */
  gcc_assert (m1_size <= m2_size);

  /* (set (strict_low_part (subreg:M1 (reg:M2 X) N)) ...)
      
  This is a bitfield insertion.  The assignment clobbers exactly the
  bits named by the subreg--the M1 bits at position N.  It is also
  technically possible that the bits asked for are longer than units
  per word.  */
  
  start = offset;
  last = offset + m1_size;

  if (dump_file)
    fprintf (dump_file, "    cpb strict low part regno=%d start=%d last=%d\n", 
	     DF_REF_REGNO (ref), start, last);

  *start_byte = start;
  *last_byte = last;
  return false;
}

/* Helper for df_compute_accessed_bytes.  Ref is a naked subreg.
   Return true if this effects the entire reg in REF. Return false if
   otherwise and set START_BYTE and LAST_BYTE.  */ 

static bool 
df_compute_accessed_bytes_subreg (df_ref ref, unsigned int *start_byte, 
				  unsigned int *last_byte)

{
  /* (subreg:M1 (reg:M2 X) N) */
  int start;
  int last;
  rtx reg = DF_REF_REG (ref);

  enum machine_mode m1;
  int m1_size;
  enum machine_mode m2;
  int m2_size;

  /* In order to accommodate multiword subregs of a hardreg, df_scan
     eats the subreg and it can only be found from the loc.  */
  if (REG_P (reg))
    reg = *(DF_REF_LOC (ref));

  m1 = GET_MODE (reg);
  m1_size = GET_MODE_SIZE (m1);
  m2 = GET_MODE (SUBREG_REG (reg));
  m2_size = GET_MODE_SIZE (m2);

  /* A simple paradoxical subreg just accesses the entire inner reg.  */
  if (m1_size >= m2_size)
    return true;

  /* Defs and uses are different in the amount of the reg that touch.  */
  if (DF_REF_REG_DEF_P (ref))
    {
      /* This is an lvalue.  */ 

      if (m2_size > UNITS_PER_WORD)
	{
	  /* The assignment clobbers UNITS_PER_WORD segments of X.
	     Look at the bytes named by the subreg, and expand it to
	     cover a UNITS_PER_WORD part of register X.  That part of
	     register X is clobbered, the rest is not.
	     
	     E.g., (subreg:SI (reg:DI X) 0), where UNITS_PER_WORD is the
	     size of SImode, clobbers the first SImode part of X, and does
	     not affect the second SImode part.
	     
	     E.g., (subreg:QI (reg:DI X) 0), where UNITS_PER_WORD is the
	     size of SImode, clobbers the first SImode part of X, and does
	     not affect the second SImode part.  Here the QImode byte is
	     expanded to a UNITS_PER_WORD portion of the register for
	     purposes of determining what is clobbered.
	     
	     If this is an rvalue, then it touches just the bytes that it
	     talks about.  */
	  int offset = SUBREG_BYTE (reg);
	  
	  start = offset & ~(UNITS_PER_WORD - 1);
	  last = (offset + m1_size + UNITS_PER_WORD - 1) 
	    & ~(UNITS_PER_WORD - 1);
	}
      else
	/* Whole register size M2 equal to or smaller than
	   UNITS_PER_WORD The assignment clobbers the entire register
	   X.  */
	return true;
    }
  else 
    {
      /* This is an rvalue. It touches just the bytes they explicitly
	 mentioned.  */
      int offset = SUBREG_BYTE (reg);
      start = offset;
      last = start + m1_size;
    }
  
  if (dump_file)
    fprintf (dump_file, "    cpb subreg regno=%d start=%d last=%d\n", 
	     DF_REF_REGNO (ref), start, last);

  *start_byte = start;
  *last_byte = last;
  return false;
}


/* Compute the set of affected bytes by a store to a pseudo to REF.
   MM is either DF_MM_MAY or DF_MM_MUST.  This is only relevant for
   the extracts which are not aligned to byte boundaries.  The
   DF_MM_MAY returns all of the bytes that any bit is set in and the
   DF_MM_MUST returns only the bytes that are completely covered.  In
   general DF_MM_MAY is used for uses and DF_MM_MUST is used for defs,
   but there are exceptions such as the inner loop of the byte level
   dead code eliminator which needs DF_MM_MAY for the defs to see if
   it any possible bit could be used.

   If the store is to the whole register, just return TRUE, if it is
   to part of the register, return FALSE and set START_BYTE and
   LAST_BYTE properly.  In the case where fabricated uses are passed
   in, START_BYTE and LAST_BYTE are set to 0 and false is returned.
   This means that this use can be ignored.  */

bool 
df_compute_accessed_bytes (df_ref ref, enum df_mm mm, 
			   unsigned int *start_byte, 
			   unsigned int *last_byte)
{
  if (!dbg_cnt (df_byte_scan))
    return true;

  if (!DF_REF_REG_DEF_P (ref) 
      && DF_REF_FLAGS_IS_SET (ref, DF_REF_READ_WRITE))
    {
      if (DF_REF_FLAGS_IS_SET (ref, DF_REF_PRE_POST_MODIFY))
	/* Pre/post modify/inc/dec always read and write the entire
	   reg.  */
	return true;
      else
	{
	  /* DF_REF_READ_WRITE on a use (except for the
	     DF_REF_PRE_POST_MODIFY) means that this use is fabricated
	     from a def that is a partial set to a multiword reg.
	     Here, we only model those cases precisely so the only one
	     to consider is the use put on a auto inc and dec
	     insns.  */
	  *start_byte = 0;
	  *last_byte = 0;
	  return false;
	}
    }

  if (DF_REF_FLAGS_IS_SET (ref, DF_REF_SIGN_EXTRACT | DF_REF_ZERO_EXTRACT))
    return df_compute_accessed_bytes_extract (ref, mm, start_byte, last_byte);
  else if (DF_REF_FLAGS_IS_SET (ref, DF_REF_STRICT_LOW_PART))
    return df_compute_accessed_bytes_strict_low_part (ref, 
						      start_byte, last_byte);
  else if (GET_CODE (DF_REF_REG (ref)) == SUBREG)
    return df_compute_accessed_bytes_subreg (ref, start_byte, last_byte);
  return true;
}

