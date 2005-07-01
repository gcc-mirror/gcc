/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                   T R A C E B A C K - A l p h a / V M S                  *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *            Copyright (C) 2003,2005 Ada Core Technologies, Inc            *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
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

#include <vms/pdscdef.h>
#include <vms/libicb.h>
#include <vms/chfctxdef.h>
#include <vms/chfdef.h>

/* A couple of items missing from the header file included above.  */
extern void * SYS$GL_CALL_HANDL;
#define PDSC$M_BASE_FRAME (1 << 10)

/* Registers are 64bit wide and addresses are 32bit wide on alpha-vms.  */
typedef void * ADDR;
typedef unsigned long long REG;

#define REG_AT(addr) (*(REG *)(addr))

#define AS_REG(addr) ((REG)(unsigned long)(addr))
#define AS_ADDR(reg) ((ADDR)(unsigned long)(reg))
#define ADDR_IN(reg) (AS_ADDR(reg))

/* The following structure defines the state maintained during the
   unwinding process.  */
typedef struct
{
  ADDR pc;  /* Address of the call insn involved in the chain.  */
  ADDR sp;  /* Stack Pointer at the time of this call.  */
  ADDR fp;  /* Frame Pointer at the time of this call.  */

  /* The values above are fetched as saved REGisters on the stack. They are
     typed ADDR because this is what the values in those registers are.  */

  /* Values of the registers saved by the functions in the chain,
     incrementally updated through consecutive calls to the "unwind" function
     below.  */
  REG saved_regs [32];
} frame_state_t;

/* Shortcuts for saved_regs of specific interest:

   Frame Pointer   is r29,
   Stack Pointer   is r30,
   Return Address  is r26,
   Procedure Value is r27.

   This is from ABI-3.1.1 [Integer Registers].  */

#define saved_fpr saved_regs[29]
#define saved_spr saved_regs[30]
#define saved_rar saved_regs[26]
#define saved_pvr saved_regs[27]

/* Special values for saved_rar, used to control the overall unwinding
   process.  */
#define RA_UNKNOWN ((REG)~0)
#define RA_STOP    ((REG)0)

/* We still use a number of macros similar to the ones for the generic
   __gnat_backtrace implementation.  */
#define PC_ADJUST 4
#define STOP_FRAME (frame_state.saved_rar == RA_STOP)

/* Compute Procedure Value from Frame Pointer value.  This follows the rules
   in ABI-3.6.1 [Current Procedure].  */
#define PV_FOR(FP) \
  (((FP) != 0) \
    ? (((REG_AT (FP) & 0x7) == 0) ? *(PDSCDEF **)(FP) : (PDSCDEF *)(FP)) : 0)


/**********
 * unwind *
 **********/

/* Helper for __gnat_backtrace.

   FS represents some call frame, identified by a pc and associated frame
   pointer in FS->pc and FS->fp. FS->saved_regs contains the state of the
   general registers upon entry in this frame. Of most interest in this set
   are the saved return address and frame pointer registers, which actually
   allow identifying the caller's frame.

   This routine "unwinds" the input frame state by adjusting it to eventually
   represent its caller's frame. The basic principle is to shift the fp and pc
   saved values into the current state, and then compute the corresponding new
   saved registers set.

   If the call chain goes through a signal handler, special processing is
   required when we process the kernel frame which has called the handler, to
   switch it to the interrupted context frame.  */

#define K_HANDLER_FRAME(fs) (PV_FOR ((fs)->fp) == SYS$GL_CALL_HANDL)

static void unwind_regular_code (frame_state_t * fs);
static void unwind_kernel_handler (frame_state_t * fs);

void
unwind (frame_state_t * fs)
{
  /* Don't do anything if requested so.  */
  if (fs->saved_rar == RA_STOP)
    return;

  /* Retrieve the values of interest computed during the previous
     call. PC_ADJUST gets us from the return address to the call insn
     address.  */
  fs->pc = ADDR_IN (fs->saved_rar) - PC_ADJUST;
  fs->sp = ADDR_IN (fs->saved_spr);
  fs->fp = ADDR_IN (fs->saved_fpr);

  /* Unless we are able to determine otherwise, set the frame state's
     saved return address such that the unwinding process will stop.  */
  fs->saved_rar = RA_STOP;

  /* Now we want to update fs->saved_regs to reflect the state of the caller
     of the procedure described by pc/fp.

     The condition to check for a special kernel frame which has called a
     signal handler is stated in ABI-6.7.1 [Signaler's Registers] : "The frame
     of the call to the handler can be identified by the return address of
     SYS$CALL_HANDL+4". We use the equivalent procedure value identification
     here because SYS$CALL_HANDL appears to be undefined. */

  if (K_HANDLER_FRAME (fs))
    unwind_kernel_handler (fs);
  else
    unwind_regular_code (fs);
}

/***********************
 * unwind_regular_code *
 ***********************/

/* Helper for unwind, for the case of unwinding through regular code which
   is not a signal handler.  */

static void
unwind_regular_code (frame_state_t * fs)
{
  PDSCDEF * pv = PV_FOR (fs->fp);

  ADDR frame_base;

  /* Use the procedure value to unwind, in a way depending on the kind of
     procedure at hand. See ABI-3.3 [Procedure Representation] and ABI-3.4
     [Procedure Types].  */

  if (pv == 0
      || pv->pdsc$w_flags & PDSC$M_BASE_FRAME)
    return;

  frame_base
    = (pv->pdsc$w_flags & PDSC$M_BASE_REG_IS_FP) ? fs->fp : fs->sp;

  switch (pv->pdsc$w_flags & 0xf)
    {
    case PDSC$K_KIND_FP_STACK:
      /* Stack Frame Procedure (ABI-3.4.1). Retrieve the necessary registers
	 from the Register Save Area in the frame.  */
      {
	ADDR rsa_base = frame_base + pv->pdsc$w_rsa_offset;
	int i, j;

	fs->saved_rar = REG_AT (rsa_base);
	fs->saved_pvr = REG_AT (frame_base);

	for (i = 0, j = 0; i < 32; i++)
	  if (pv->pdsc$l_ireg_mask & (1 << i))
	    fs->saved_regs[i] = REG_AT (rsa_base + 8 * ++j);

	/* Note that the loop above is guaranteed to set fs->saved_fpr,
	   because "The preserved register set must always include R29(FP)
	   since it will always be used." (ABI-3.4.3.4 [Register Save Area for
	   All Stack Frames]).

	   Also note that we need to run through all the registers to ensure
	   that unwinding through register procedures (see below) gets the
	   right values out of the saved_regs array.  */
      }
      break;

    case PDSC$K_KIND_FP_REGISTER:
      /* Register Procedure (ABI-3.4.4). Retrieve the necessary registers from
	 the registers where they have been saved.  */
      {
	fs->saved_rar = fs->saved_regs[pv->pdsc$b_save_ra];
	fs->saved_fpr = fs->saved_regs[pv->pdsc$b_save_fp];
      }
      break;

    default:
      /* ??? Are we supposed to ever get here ?  Don't think so.  */
      break;
    }

  /* SP is actually never part of the saved registers area, so we use the
     corresponding entry in the saved_regs array to manually keep track of
     it's evolution.  */
  fs->saved_spr = AS_REG (frame_base) + pv->pdsc$l_size;
}

/*************************
 * unwind_kernel_handler *
 *************************/

/* Helper for unwind, for the specific case of unwinding through a signal
   handler.

   The input frame state describes the kernel frame which has called a signal
   handler. We fill the corresponding saved_regs to have it's "caller" frame
   represented as the interrupted context.  */

static void
unwind_kernel_handler (frame_state_t * fs)
{
  PDSCDEF * pv = PV_FOR (fs->fp);

  CHFDEF1 *sigargs;
  CHFDEF2 *mechargs;

  /* Retrieve the arguments passed to the handler, by way of a VMS service
     providing the corresponding "Invocation Context Block".  */
  {
    long handler_ivhandle;
    INVO_CONTEXT_BLK handler_ivcb;

    CHFCTX *chfctx;

    handler_ivcb.libicb$q_ireg [29] = AS_REG (fs->fp);
    handler_ivcb.libicb$q_ireg [30] = 0;

    handler_ivhandle = LIB$GET_INVO_HANDLE (&handler_ivcb);

    if ((LIB$GET_INVO_CONTEXT (handler_ivhandle, &handler_ivcb) & 1) != 1)
      return;

    chfctx = (CHFCTX *) AS_ADDR (handler_ivcb.libicb$ph_chfctx_addr);

    sigargs = (CHFDEF1 *) AS_ADDR (chfctx->chfctx$q_sigarglst);
    mechargs = (CHFDEF2 *) AS_ADDR (chfctx->chfctx$q_mcharglst);
  }

  /* Compute the saved return address as the PC of the instruction causing the
     condition, accounting for the fact that it will be adjusted by the next
     call to "unwind" as if it was an actual call return address.  */
  {
    /* ABI-6.5.1.1 [Signal Argument Vector]: The signal occurrence address
       is available from the sigargs argument to the handler, designed to
       support both 32 and 64 bit addresses.  The initial reference we get
       is a pointer to the 32bit form, from which one may extract a pointer
       to the 64bit version if need be.  We work directly from the 32bit
       form here.  */

    /* The sigargs vector structure for 32bits addresses is:

       <......32bit......>
       +-----------------+
       |      Vsize      | :chf$is_sig_args
       +-----------------+ -+-
       | Condition Value |  : [0]
       +-----------------+  :
       |       ...       |  :
       +-----------------+  : vector of Vsize entries
       |    Signal PC    |  :
       +-----------------+  :
       |       PS        |  : [Vsize - 1]
       +-----------------+ -+-

       */

    unsigned long * sigargs_vector
      = ((unsigned long *) (&sigargs->chf$is_sig_args)) + 1;

    long sigargs_vsize
      = sigargs->chf$is_sig_args;

    fs->saved_rar = (REG) sigargs_vector [sigargs_vsize - 2] + PC_ADJUST;
  }

  fs->saved_spr = RA_UNKNOWN;
  fs->saved_fpr = (REG) mechargs->chf$q_mch_frame;
  fs->saved_pvr = (REG) mechargs->chf$q_mch_savr27;

  fs->saved_regs[16] = (REG) mechargs->chf$q_mch_savr16;
  fs->saved_regs[17] = (REG) mechargs->chf$q_mch_savr17;
  fs->saved_regs[18] = (REG) mechargs->chf$q_mch_savr18;
  fs->saved_regs[19] = (REG) mechargs->chf$q_mch_savr19;
  fs->saved_regs[20] = (REG) mechargs->chf$q_mch_savr20;
}

/* Structure representing a traceback entry in the tracebacks array to be
   filled by __gnat_backtrace below.

   !! This should match what is in System.Traceback_Entries, so beware of
   !! the REG/ADDR difference here.

   The use of a structure is motivated by the potential necessity of having
   several fields to fill for each entry, for instance if later calls to VMS
   system functions need more than just a mere PC to compute info on a frame
   (e.g. for non-symbolic->symbolic translation purposes).  */
typedef struct {
  ADDR pc;
  ADDR pv;
} tb_entry_t;

/********************
 * __gnat_backtrace *
 ********************/

int
__gnat_backtrace (void **array, int size,
                  void *exclude_min, void *exclude_max, int skip_frames)
{
  int cnt;

  tb_entry_t * tbe = (tb_entry_t *)&array [0];

  frame_state_t frame_state;

  /* Setup the frame state before initiating the unwinding sequence.  */
  register REG this_FP __asm__("$29");
  register REG this_SP __asm__("$30");

  frame_state.saved_fpr = this_FP;
  frame_state.saved_spr = this_SP;
  frame_state.saved_rar = RA_UNKNOWN;

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
      PDSCDEF * pv = PV_FOR (frame_state.fp);

      /* Stop if either the frame contents or the unwinder say so.  */
      if (STOP_FRAME)
        break;

      if (! K_HANDLER_FRAME (&frame_state)
	  && (frame_state.pc < exclude_min || frame_state.pc > exclude_max))
	{
	  tbe->pc = (ADDR) frame_state.pc;
	  tbe->pv = (ADDR) PV_FOR (frame_state.fp);

	  cnt ++;
	  tbe ++;
	}

      unwind (&frame_state);
    }

  return cnt;
}
