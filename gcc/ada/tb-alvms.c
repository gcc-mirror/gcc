/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                   T R A C E B A C K - A l p h a / V M S                  *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *              Copyright (C) 2003 Ada Core Technologies, Inc               *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/


/* Alpha VMS requires a special treatment due to the complexity of the ABI.
   What is here is along the lines of what the MD_FALLBACK_FRAME_STATE_FOR
   macro does for frame unwinding during exception propagation. This file is
   #included within tracebak.c in the appropriate case.

   Most of the contents is directed by the OpenVMS/Alpha Conventions (ABI)
   document, sections of which we will refer to as ABI-<section_number>.  */

#include <pdscdef.h>

/* We still use a number of macros similar to the ones for the generic
   __gnat_backtrace implementation.  */
#define SKIP_FRAME 1
#define PC_ADJUST -4

#define STOP_FRAME (frame_state.saved_ra == RA_STOP)

/* Mask for PDSC$V_BASE_FRAME in procedure descriptors, missing from the
   header file included above.  */
#define PDSC$M_BASE_FRAME (1 << 10)

typedef unsigned long REG;

#define REG_AT(address) (*(REG *)(address))

/* The following structure defines the state maintained during the
   unwinding process.  */
typedef struct
{
  void * pc;  /* Address of the call insn involved in the chain.  */
  void * sp;  /* Stack Pointer at the time of this call.  */
  void * fp;  /* Frame Pointer at the time of this call.  */

  /* Values of the registers saved by the functions in the chain,
     incrementally updated through consecutive calls to the "unwind"
     function below.  */
  REG saved_regs [32];
} frame_state_t;

/* Shortcuts for saved_regs of specific interest:

   Frame Pointer   is r29,
   Stack Pointer   is r30,
   Return Address  is r26,
   Procedure Value is r27.

   This is from ABI-3.1.1 [Integer Registers].  */

#define saved_fp saved_regs[29]
#define saved_sp saved_regs[30]
#define saved_ra saved_regs[26]
#define saved_pv saved_regs[27]

/* Special values for saved_ra, used to control the overall unwinding
   process.  */
#define RA_UNKNOWN ((REG)~0)
#define RA_STOP    ((REG)0)

/* Compute Procedure Value from a live Frame Pointer value.  */
#define PV_FOR(FP) \
  ((REG_AT (FP) & 0x7) == 0) ? *(PDSCDEF **)(FP) : (PDSCDEF *)(FP);

/**********
 * unwind *
 **********/

/* Helper for __gnat_backtrace. Update FS->pc/sp/fp to represent the
   state computed in FS->saved_regs during the previous call, and update
   FS->saved_regs in preparation of the next call.  */

void
unwind (frame_state_t * fs)
{
  REG frame_base;
  PDSCDEF * pv;

  /* Don't do anything if requested so.  */
  if (fs->saved_ra == RA_STOP)
    return;

  /* Retrieve the values of interest computed during the previous
     call. PC_ADJUST gets us from the return address to the call insn
     address.  */
  fs->pc = (void *) fs->saved_ra + PC_ADJUST;
  fs->sp = (void *) fs->saved_sp;
  fs->fp = (void *) fs->saved_fp;

  /* Unless we are able to determine otherwise, set the frame state's
     saved return address such that the unwinding process will stop.  */
  fs->saved_ra = RA_STOP;

  /* Now we want to update fs->saved_regs to reflect what the procedure
     described by pc/fp/sp has done.  */

  /* Compute the corresponding "procedure value", following the rules in
     ABI-3.6.1 [Current Procedure]. Return immediatly if this value mandates
     us to stop.  */
  if (fs->fp == 0)
    return;

  pv = PV_FOR (fs->fp);

  if (pv == 0
      || pv->pdsc$w_flags & PDSC$M_BASE_FRAME)
    return;

  /* Use the procedure value to unwind, in a way depending on the kind of
     procedure at hand. This is based on ABI-3.3 [Procedure Representation]
     and ABI-3.4 [Procedure Types].  */
  frame_base
    = (REG) ((pv->pdsc$w_flags & PDSC$M_BASE_REG_IS_FP) ? fs->fp : fs->sp);

  switch (pv->pdsc$w_flags & 0xf)
    {
    case PDSC$K_KIND_FP_STACK:
      /* Stack Frame Procedure (ABI-3.4.1). Retrieve the necessary registers
	 from the Register Save Area in the frame.  */
      {
	REG rsa_base = frame_base + pv->pdsc$w_rsa_offset;
	int i, j;

	fs->saved_ra = REG_AT (rsa_base);
	fs->saved_pv = REG_AT (frame_base);
	
	for (i = 0, j = 0; i < 32; i++)
	  if (pv->pdsc$l_ireg_mask & (1 << i))
	    fs->saved_regs[i] = REG_AT (rsa_base + 8 * ++j);

	/* Note that the loop above is guaranteed to set fs->saved_fp, because
	   "The preserved register set must always include R29(FP) since it
	   will always be used." (ABI-3.4.3.4 [Register Save Area for All
	   Stack Frames]).
	
	   Also note that we need to run through all the registers to ensure
	   that unwinding through register procedures (see below) gets the
	   right values out of the saved_regs array.  */
      }
      break;

    case PDSC$K_KIND_FP_REGISTER:
      /* Register Procedure (ABI-3.4.4). Retrieve the necessary registers from
	 the registers where they have been saved.  */
      {
	fs->saved_ra = fs->saved_regs[pv->pdsc$b_save_ra];
	fs->saved_fp = fs->saved_regs[pv->pdsc$b_save_fp];
      }
      break;

    default:
      /* ??? Are we supposed to ever get here ?  Don't think so.  */
      break;
    }

  /* SP is actually never part of the saved registers area, so we use the
     corresponding entry in the saved_regs array to manually keep track of
     it's evolution.  */
  fs->saved_sp = frame_base + pv->pdsc$l_size;
}

/* Structure representing a traceback entry in the tracebacks array to be
   filled by __gnat_backtrace below.

   The use of a structure is motivated by the potential necessity of having
   several fields to fill for each entry, for instance if later calls to VMS
   system functions need more than just a mere PC to compute info on a frame
   (e.g. for non-symbolic->symbolic translation purposes).  */
typedef struct {
  void * pc;
  void * pv;
} tb_entry_t;

/********************
 * __gnat_backtrace *
 ********************/

int
__gnat_backtrace (array, size, exclude_min, exclude_max, skip_frames)
     void **array;
     int size;
     void *exclude_min;
     void *exclude_max;
     int skip_frames;
{
  int cnt;

  tb_entry_t * tbe = (tb_entry_t *)&array [0];

  frame_state_t frame_state;

  /* Setup the frame state before initiating the unwinding sequence.  */
  register REG this_FP __asm__("$29");
  register REG this_SP __asm__("$30");

  frame_state.saved_fp = this_FP;
  frame_state.saved_sp = this_SP;
  frame_state.saved_ra = RA_UNKNOWN;

  unwind (&frame_state);

  /* At this point frame_state describes this very function. Skip the
     requested number of calls.  */
  for (cnt = 0; cnt < skip_frames; cnt ++)
    unwind (&frame_state);

  /* Now consider each frame as a potential candidate for insertion inside
     the provided array.  */
  cnt = 0;
  while (cnt < size)
    {
      if (STOP_FRAME)
        break;

      if (frame_state.pc < exclude_min
	  || frame_state.pc > exclude_max)
	{
	  tbe->pc = frame_state.pc;
	  tbe->pv = PV_FOR (frame_state.fp);
	
	  cnt ++;
	  tbe ++;
	}

      unwind (&frame_state);
    }

  return cnt;
}
