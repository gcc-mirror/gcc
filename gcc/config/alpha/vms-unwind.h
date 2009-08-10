/* Fallback frame unwinding for Alpha/VMS.
   Copyright (C) 1996, 1997, 1998, 2000, 2001, 2002, 2003, 2009
   Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include <stdlib.h>
#include <stdio.h>
#include <vms/pdscdef.h>
#include <vms/libicb.h>
#include <vms/chfctxdef.h>
#include <vms/chfdef.h>

#define MD_FALLBACK_FRAME_STATE_FOR alpha_vms_fallback_frame_state

typedef void * ADDR;
typedef unsigned long long REG;
typedef PDSCDEF * PV;

#define REG_AT(addr) (*(REG *)(addr))
#define ADDR_AT(addr) (*(ADDR *)(addr))

/* Compute pointer to procedure descriptor (Procedure Value) from Frame
   Pointer FP, according to the rules in [ABI-3.5.1 Current Procedure].  */
#define PV_FOR(FP) \
  (((FP) != 0) \
    ? (((REG_AT (FP) & 0x7) == 0) ? *(PDSCDEF **)(FP) : (PDSCDEF *)(FP)) : 0)

extern int SYS$GL_CALL_HANDL;
/* This is actually defined as a "long", but in system code where longs
   are always 4bytes while GCC longs might be 8bytes.  */

#define UPDATE_FS_FOR_CFA_GR(FS, GRN, LOC, CFA) \
do { \
(FS)->regs.reg[GRN].how = REG_SAVED_OFFSET;      \
(FS)->regs.reg[GRN].loc.offset = (_Unwind_Sword) ((REG) (LOC) - (REG) (CFA)); \
} while (0);

#define GIVEUP_ON_FAILURE(STATUS) \
  { if ((((STATUS) & 1) != 1)) return _URC_END_OF_STACK; }
#define DENOTES_EXC_DISPATCHER(PV) ((PV) == (ADDR) (REG) SYS$GL_CALL_HANDL)

#define RA_COLUMN (DWARF_ALT_FRAME_RETURN_COLUMN)

static int
alpha_vms_fallback_frame_state (struct _Unwind_Context *context,
				_Unwind_FrameState *fs)
{
  static int eh_debug = -1;

  /* Our goal is to update FS to reflect the state one step up CONTEXT, that
     is: the CFA, return address and *saved* registers locations associated
     with the function designated by CONTEXT->ra.  We are called when the
     libgcc unwinder has not found any dwarf FDE for this address, which
     typically happens when trying to propagate a language exception through a
     signal global vector or frame based handler.

     The CONTEXT->reg[] entries reflect the state/location of register saves
     so designate values live at the CONTEXT->ra point.  Of precious value to
     us here is the frame pointer (r29), which gets us a procedure value.  */

  PV pv = (context->reg[29] != 0) ? PV_FOR (ADDR_AT (context->reg[29])) : 0;

  int pkind = pv ? pv->pdsc$w_flags & 0xf : 0;
  /* VMS procedure kind, as indicated by the procedure descriptor.  We only
     know how to deal with FP_STACK or FP_REGISTER here.  */

  ADDR new_cfa = 0;
  /* CFA we will establish for the caller, computed in different ways,
     e.g. depending whether we cross an exception dispatcher frame.  */

  CHFCTX *chfctx = 0;
  /* Pointer to the VMS CHF context associated with an exception dispatcher
     frame, if we happen to come across one.  */

  int i,j;

  if (eh_debug == -1)
    {
      char * eh_debug_env = getenv ("EH_DEBUG");
      eh_debug = eh_debug_env ? atoi (eh_debug_env) : 0;
    }

  if (eh_debug)
    printf ("MD_FALLBACK running ...\n");

  /* We only know how to deal with stack or reg frame procedures, so give
     up if we're handed anything else.  */
  if (pkind != PDSC$K_KIND_FP_STACK && pkind != PDSC$K_KIND_FP_REGISTER)
    return _URC_END_OF_STACK;
  
  if (eh_debug)
    printf ("FALLBACK: CTX FP = 0x%p, PV = 0x%p, EN = 0x%llx, RA = 0x%p\n",
	    ADDR_AT (context->reg[29]), pv, pv->pdsc$q_entry, context->ra);

  fs->retaddr_column = RA_COLUMN;

  /* If PV designates a VMS exception vector or condition handler, we need to
     do as if the caller was the signaling point and estabish the state of the
     intermediate VMS code (CFA, RA and saved register locations) as if it was
     a single regular function.  This requires special processing.

     The datastructures available from an condition dispatcher frame (signal
     context) do not contain the values of most callee-saved registers, so
     whathever PV designates, we need to account for the registers it saves.

     Besides, we need to express all the locations with respect to a
     consistent CFA value, so we compute this first.  */

  if (DENOTES_EXC_DISPATCHER (pv))
    {
      /* The CFA to establish is the signaling point's stack pointer. We
	 compute it using the system invocation context unwinding services and
	 save the CHF context data pointer along the way for later uses.  */

      INVO_CONTEXT_BLK icb;
      int status, invo_handle;

      if (eh_debug)
	printf ("FALLBACK: SYS$HANDLER\n");

      icb.libicb$q_ireg [29] = REG_AT (context->reg[29]);
      icb.libicb$q_ireg [30] = 0;
      invo_handle = LIB$GET_INVO_HANDLE (&icb);

      status = LIB$GET_INVO_CONTEXT (invo_handle, &icb);
      GIVEUP_ON_FAILURE (status);

      chfctx = (CHFCTX *) icb.libicb$ph_chfctx_addr;

      status = LIB$GET_PREV_INVO_CONTEXT (&icb);
      GIVEUP_ON_FAILURE (status);

      new_cfa = (ADDR) icb.libicb$q_ireg[30];      
    }
  else
    {
      /* The CFA to establish is the SP value on entry of the procedure
	 designated by PV, which we compute as the corresponding frame base
	 register value + frame size.  Note that the frame base may differ
	 from CONTEXT->cfa, typically if the caller has performed dynamic
	 stack allocations.  */
      
      int  base_reg  = pv->pdsc$w_flags & PDSC$M_BASE_REG_IS_FP ? 29 : 30;
      ADDR base_addr = ADDR_AT (context->reg[base_reg]);
      
      new_cfa = base_addr + pv->pdsc$l_size;
    }

  /* State to compute the caller's CFA by adding an offset to the current
     one in CONTEXT.  */
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __builtin_dwarf_sp_column ();
  fs->regs.cfa_offset = new_cfa - context->cfa;

  /* Regular unwind first, accounting for the register saves performed by
     the procedure designated by PV.  */

  switch (pkind)
    {
    case PDSC$K_KIND_FP_STACK:
      {
	/* The saved registers are all located in the Register Save Area,
	   except for the procedure value register (R27) found at the frame
	   base address.  */

	int  base_reg  = pv->pdsc$w_flags & PDSC$M_BASE_REG_IS_FP ? 29 : 30;
	ADDR base_addr = ADDR_AT (context->reg[base_reg]);
	ADDR rsa_addr  = base_addr + pv->pdsc$w_rsa_offset;

	if (eh_debug)
	  printf ("FALLBACK: STACK frame procedure\n");

	UPDATE_FS_FOR_CFA_GR (fs, 27, base_addr, new_cfa);

	/* The first RSA entry is for the return address register, R26.  */

	UPDATE_FS_FOR_CFA_GR (fs, 26, rsa_addr, new_cfa);
	UPDATE_FS_FOR_CFA_GR (fs, RA_COLUMN, rsa_addr, new_cfa);

	/* The following entries are for registers marked as saved according
	   to ireg_mask.  */
	for (i = 0, j = 0; i < 32; i++)
	  if ((1 << i) & pv->pdsc$l_ireg_mask)
	    UPDATE_FS_FOR_CFA_GR (fs, i, rsa_addr + 8 * ++j, new_cfa);
	
	/* ??? floating point registers ?  */

	break;
      }

    case PDSC$K_KIND_FP_REGISTER:
      {
	if (eh_debug)
	  printf ("FALLBACK: REGISTER frame procedure\n");

	fs->regs.reg[RA_COLUMN].how = REG_SAVED_REG;
	fs->regs.reg[RA_COLUMN].loc.reg = pv->pdsc$b_save_ra;
	
	fs->regs.reg[29].how = REG_SAVED_REG;
	fs->regs.reg[29].loc.reg = pv->pdsc$b_save_fp;
	
	break;
      }

    default:
      /* Should never reach here.  */
      return _URC_END_OF_STACK;
    }

  /* If PV designates an exception dispatcher, we have to adjust the return
     address column to get at the signal occurrence point, and account for
     for what the CHF context contains.  */

  if (DENOTES_EXC_DISPATCHER (pv))
    {
      /* The PC of the instruction causing the condition is available from the
	 signal argument vector.  Extra saved register values are available
	 from the mechargs array.  */

      CHF$SIGNAL_ARRAY *sigargs
	= (CHF$SIGNAL_ARRAY *) chfctx->chfctx$q_sigarglst;

      CHF$MECH_ARRAY *mechargs
	= (CHF$MECH_ARRAY *) chfctx->chfctx$q_mcharglst;

      ADDR condpc_addr
	= &((int *)(&sigargs->chf$l_sig_name)) [sigargs->chf$is_sig_args-2];

      ADDR rei_frame_addr = (void *) mechargs->chf$q_mch_esf_addr;

      /* Adjust the return address location.  */

      UPDATE_FS_FOR_CFA_GR (fs, RA_COLUMN, condpc_addr, new_cfa);

      /* The frame pointer at the condition point is available from the
	 chf context directly.  */

      UPDATE_FS_FOR_CFA_GR (fs, 29, &chfctx->chfctx$q_expt_fp, new_cfa);

      /* Registers available from the mechargs array.  */

      UPDATE_FS_FOR_CFA_GR (fs, 0, &mechargs->chf$q_mch_savr0, new_cfa);
      UPDATE_FS_FOR_CFA_GR (fs, 1, &mechargs->chf$q_mch_savr1, new_cfa);

      UPDATE_FS_FOR_CFA_GR (fs, 16, &mechargs->chf$q_mch_savr16, new_cfa);
      UPDATE_FS_FOR_CFA_GR (fs, 17, &mechargs->chf$q_mch_savr17, new_cfa);
      UPDATE_FS_FOR_CFA_GR (fs, 18, &mechargs->chf$q_mch_savr18, new_cfa);
      UPDATE_FS_FOR_CFA_GR (fs, 19, &mechargs->chf$q_mch_savr19, new_cfa);
      UPDATE_FS_FOR_CFA_GR (fs, 20, &mechargs->chf$q_mch_savr20, new_cfa);
      UPDATE_FS_FOR_CFA_GR (fs, 21, &mechargs->chf$q_mch_savr21, new_cfa);
      UPDATE_FS_FOR_CFA_GR (fs, 22, &mechargs->chf$q_mch_savr22, new_cfa);
      UPDATE_FS_FOR_CFA_GR (fs, 23, &mechargs->chf$q_mch_savr23, new_cfa);
      UPDATE_FS_FOR_CFA_GR (fs, 24, &mechargs->chf$q_mch_savr24, new_cfa);
      UPDATE_FS_FOR_CFA_GR (fs, 25, &mechargs->chf$q_mch_savr25, new_cfa);
      UPDATE_FS_FOR_CFA_GR (fs, 26, &mechargs->chf$q_mch_savr26, new_cfa);
      UPDATE_FS_FOR_CFA_GR (fs, 27, &mechargs->chf$q_mch_savr27, new_cfa);
      UPDATE_FS_FOR_CFA_GR (fs, 28, &mechargs->chf$q_mch_savr28, new_cfa);
      
      /* Registers R2 to R7 are available from the rei frame pointer.  */
      
      for (i = 2; i <= 7; i ++)
	UPDATE_FS_FOR_CFA_GR (fs, i, rei_frame_addr+(i - 2)*8, new_cfa);
      
      /* ??? floating point registers ?  */
    }

  return _URC_NO_REASON;
}



