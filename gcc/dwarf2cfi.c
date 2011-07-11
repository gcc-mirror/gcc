/* Dwarf2 Call Frame Information helper routines.
   Copyright (C) 1992, 1993, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
   2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

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
#include "version.h"
#include "flags.h"
#include "rtl.h"
#include "function.h"
#include "dwarf2.h"
#include "dwarf2out.h"
#include "dwarf2asm.h"
#include "ggc.h"
#include "tm_p.h"
#include "target.h"
#include "common/common-target.h"
#include "tree-pass.h"

#include "except.h"		/* expand_builtin_dwarf_sp_column */
#include "expr.h"		/* init_return_column_size */
#include "regs.h"		/* expand_builtin_init_dwarf_reg_sizes */
#include "output.h"		/* asm_out_file */
#include "debug.h"		/* dwarf2out_do_frame, dwarf2out_do_cfi_asm */


/* ??? Poison these here until it can be done generically.  They've been
   totally replaced in this file; make sure it stays that way.  */
#undef DWARF2_UNWIND_INFO
#undef DWARF2_FRAME_INFO
#if (GCC_VERSION >= 3000)
 #pragma GCC poison DWARF2_UNWIND_INFO DWARF2_FRAME_INFO
#endif

#ifndef INCOMING_RETURN_ADDR_RTX
#define INCOMING_RETURN_ADDR_RTX  (gcc_unreachable (), NULL_RTX)
#endif

/* Maximum size (in bytes) of an artificially generated label.  */
#define MAX_ARTIFICIAL_LABEL_BYTES	30

/* Short-hand for commonly used register numbers.  */
#define DW_STACK_POINTER_REGNUM  dwarf_frame_regnum (STACK_POINTER_REGNUM)
#define DW_FRAME_POINTER_REGNUM  dwarf_frame_regnum (HARD_FRAME_POINTER_REGNUM)

/* A vector of call frame insns for the CIE.  */
cfi_vec cie_cfi_vec;

static GTY(()) unsigned long dwarf2out_cfi_label_num;

/* The insn after which a new CFI note should be emitted.  */
static rtx cfi_insn;

/* When non-null, add_cfi will add the CFI to this vector.  */
static cfi_vec *add_cfi_vec;

/* True if remember_state should be emitted before following CFI directive.  */
static bool emit_cfa_remember;

/* True if any CFI directives were emitted at the current insn.  */
static bool any_cfis_emitted;


static void dwarf2out_cfi_begin_epilogue (rtx insn);
static void dwarf2out_frame_debug_restore_state (void);


/* Hook used by __throw.  */

rtx
expand_builtin_dwarf_sp_column (void)
{
  unsigned int dwarf_regnum = DW_STACK_POINTER_REGNUM;
  return GEN_INT (DWARF2_FRAME_REG_OUT (dwarf_regnum, 1));
}

/* MEM is a memory reference for the register size table, each element of
   which has mode MODE.  Initialize column C as a return address column.  */

static void
init_return_column_size (enum machine_mode mode, rtx mem, unsigned int c)
{
  HOST_WIDE_INT offset = c * GET_MODE_SIZE (mode);
  HOST_WIDE_INT size = GET_MODE_SIZE (Pmode);
  emit_move_insn (adjust_address (mem, mode, offset), GEN_INT (size));
}

/* Generate code to initialize the register size table.  */

void
expand_builtin_init_dwarf_reg_sizes (tree address)
{
  unsigned int i;
  enum machine_mode mode = TYPE_MODE (char_type_node);
  rtx addr = expand_normal (address);
  rtx mem = gen_rtx_MEM (BLKmode, addr);
  bool wrote_return_column = false;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      unsigned int rnum = DWARF2_FRAME_REG_OUT (dwarf_frame_regnum (i), 1);

      if (rnum < DWARF_FRAME_REGISTERS)
	{
	  HOST_WIDE_INT offset = rnum * GET_MODE_SIZE (mode);
	  enum machine_mode save_mode = reg_raw_mode[i];
	  HOST_WIDE_INT size;

	  if (HARD_REGNO_CALL_PART_CLOBBERED (i, save_mode))
	    save_mode = choose_hard_reg_mode (i, 1, true);
	  if (dwarf_frame_regnum (i) == DWARF_FRAME_RETURN_COLUMN)
	    {
	      if (save_mode == VOIDmode)
		continue;
	      wrote_return_column = true;
	    }
	  size = GET_MODE_SIZE (save_mode);
	  if (offset < 0)
	    continue;

	  emit_move_insn (adjust_address (mem, mode, offset),
			  gen_int_mode (size, mode));
	}
    }

  if (!wrote_return_column)
    init_return_column_size (mode, mem, DWARF_FRAME_RETURN_COLUMN);

#ifdef DWARF_ALT_FRAME_RETURN_COLUMN
  init_return_column_size (mode, mem, DWARF_ALT_FRAME_RETURN_COLUMN);
#endif

  targetm.init_dwarf_reg_sizes_extra (address);
}

/* Divide OFF by DWARF_CIE_DATA_ALIGNMENT, asserting no remainder.  */

static inline HOST_WIDE_INT
div_data_align (HOST_WIDE_INT off)
{
  HOST_WIDE_INT r = off / DWARF_CIE_DATA_ALIGNMENT;
  gcc_assert (r * DWARF_CIE_DATA_ALIGNMENT == off);
  return r;
}

/* Return true if we need a signed version of a given opcode
   (e.g. DW_CFA_offset_extended_sf vs DW_CFA_offset_extended).  */

static inline bool
need_data_align_sf_opcode (HOST_WIDE_INT off)
{
  return DWARF_CIE_DATA_ALIGNMENT < 0 ? off > 0 : off < 0;
}

/* Return a pointer to a newly allocated Call Frame Instruction.  */

static inline dw_cfi_ref
new_cfi (void)
{
  dw_cfi_ref cfi = ggc_alloc_dw_cfi_node ();

  cfi->dw_cfi_oprnd1.dw_cfi_reg_num = 0;
  cfi->dw_cfi_oprnd2.dw_cfi_reg_num = 0;

  return cfi;
}

/* Generate a new label for the CFI info to refer to.  */

static char *
dwarf2out_cfi_label (void)
{
  int num = dwarf2out_cfi_label_num++;
  char label[20];

  ASM_GENERATE_INTERNAL_LABEL (label, "LCFI", num);

  return xstrdup (label);
}

/* Add CFI either to the current insn stream or to a vector, or both.  */

static void
add_cfi (dw_cfi_ref cfi)
{
  if (emit_cfa_remember)
    {
      dw_cfi_ref cfi_remember;

      /* Emit the state save.  */
      emit_cfa_remember = false;
      cfi_remember = new_cfi ();
      cfi_remember->dw_cfi_opc = DW_CFA_remember_state;
      add_cfi (cfi_remember);
    }

  any_cfis_emitted = true;
  if (cfi_insn != NULL)
    {
      cfi_insn = emit_note_after (NOTE_INSN_CFI, cfi_insn);
      NOTE_CFI (cfi_insn) = cfi;
    }
  if (add_cfi_vec != NULL)
    VEC_safe_push (dw_cfi_ref, gc, *add_cfi_vec, cfi);
}

/* This function fills in aa dw_cfa_location structure from a dwarf location
   descriptor sequence.  */

static void
get_cfa_from_loc_descr (dw_cfa_location *cfa, struct dw_loc_descr_struct *loc)
{
  struct dw_loc_descr_struct *ptr;
  cfa->offset = 0;
  cfa->base_offset = 0;
  cfa->indirect = 0;
  cfa->reg = -1;

  for (ptr = loc; ptr != NULL; ptr = ptr->dw_loc_next)
    {
      enum dwarf_location_atom op = ptr->dw_loc_opc;

      switch (op)
	{
	case DW_OP_reg0:
	case DW_OP_reg1:
	case DW_OP_reg2:
	case DW_OP_reg3:
	case DW_OP_reg4:
	case DW_OP_reg5:
	case DW_OP_reg6:
	case DW_OP_reg7:
	case DW_OP_reg8:
	case DW_OP_reg9:
	case DW_OP_reg10:
	case DW_OP_reg11:
	case DW_OP_reg12:
	case DW_OP_reg13:
	case DW_OP_reg14:
	case DW_OP_reg15:
	case DW_OP_reg16:
	case DW_OP_reg17:
	case DW_OP_reg18:
	case DW_OP_reg19:
	case DW_OP_reg20:
	case DW_OP_reg21:
	case DW_OP_reg22:
	case DW_OP_reg23:
	case DW_OP_reg24:
	case DW_OP_reg25:
	case DW_OP_reg26:
	case DW_OP_reg27:
	case DW_OP_reg28:
	case DW_OP_reg29:
	case DW_OP_reg30:
	case DW_OP_reg31:
	  cfa->reg = op - DW_OP_reg0;
	  break;
	case DW_OP_regx:
	  cfa->reg = ptr->dw_loc_oprnd1.v.val_int;
	  break;
	case DW_OP_breg0:
	case DW_OP_breg1:
	case DW_OP_breg2:
	case DW_OP_breg3:
	case DW_OP_breg4:
	case DW_OP_breg5:
	case DW_OP_breg6:
	case DW_OP_breg7:
	case DW_OP_breg8:
	case DW_OP_breg9:
	case DW_OP_breg10:
	case DW_OP_breg11:
	case DW_OP_breg12:
	case DW_OP_breg13:
	case DW_OP_breg14:
	case DW_OP_breg15:
	case DW_OP_breg16:
	case DW_OP_breg17:
	case DW_OP_breg18:
	case DW_OP_breg19:
	case DW_OP_breg20:
	case DW_OP_breg21:
	case DW_OP_breg22:
	case DW_OP_breg23:
	case DW_OP_breg24:
	case DW_OP_breg25:
	case DW_OP_breg26:
	case DW_OP_breg27:
	case DW_OP_breg28:
	case DW_OP_breg29:
	case DW_OP_breg30:
	case DW_OP_breg31:
	  cfa->reg = op - DW_OP_breg0;
	  cfa->base_offset = ptr->dw_loc_oprnd1.v.val_int;
	  break;
	case DW_OP_bregx:
	  cfa->reg = ptr->dw_loc_oprnd1.v.val_int;
	  cfa->base_offset = ptr->dw_loc_oprnd2.v.val_int;
	  break;
	case DW_OP_deref:
	  cfa->indirect = 1;
	  break;
	case DW_OP_plus_uconst:
	  cfa->offset = ptr->dw_loc_oprnd1.v.val_unsigned;
	  break;
	default:
	  gcc_unreachable ();
	}
    }
}

/* Find the previous value for the CFA, iteratively.  CFI is the opcode
   to interpret, *LOC will be updated as necessary, *REMEMBER is used for
   one level of remember/restore state processing.  */

void
lookup_cfa_1 (dw_cfi_ref cfi, dw_cfa_location *loc, dw_cfa_location *remember)
{
  switch (cfi->dw_cfi_opc)
    {
    case DW_CFA_def_cfa_offset:
    case DW_CFA_def_cfa_offset_sf:
      loc->offset = cfi->dw_cfi_oprnd1.dw_cfi_offset;
      break;
    case DW_CFA_def_cfa_register:
      loc->reg = cfi->dw_cfi_oprnd1.dw_cfi_reg_num;
      break;
    case DW_CFA_def_cfa:
    case DW_CFA_def_cfa_sf:
      loc->reg = cfi->dw_cfi_oprnd1.dw_cfi_reg_num;
      loc->offset = cfi->dw_cfi_oprnd2.dw_cfi_offset;
      break;
    case DW_CFA_def_cfa_expression:
      get_cfa_from_loc_descr (loc, cfi->dw_cfi_oprnd1.dw_cfi_loc);
      break;

    case DW_CFA_remember_state:
      gcc_assert (!remember->in_use);
      *remember = *loc;
      remember->in_use = 1;
      break;
    case DW_CFA_restore_state:
      gcc_assert (remember->in_use);
      *loc = *remember;
      remember->in_use = 0;
      break;

    default:
      break;
    }
}

/* The current rule for calculating the DWARF2 canonical frame address.  */
static dw_cfa_location cfa;

/* A copy of the CFA, for comparison purposes.  */
static dw_cfa_location old_cfa;

/* The register used for saving registers to the stack, and its offset
   from the CFA.  */
static dw_cfa_location cfa_store;

/* The current save location around an epilogue.  */
static dw_cfa_location cfa_remember;

/* Like cfa_remember, but a copy of old_cfa.  */
static dw_cfa_location old_cfa_remember;

/* The running total of the size of arguments pushed onto the stack.  */
static HOST_WIDE_INT args_size;

/* The last args_size we actually output.  */
static HOST_WIDE_INT old_args_size;

/* Determine if two dw_cfa_location structures define the same data.  */

bool
cfa_equal_p (const dw_cfa_location *loc1, const dw_cfa_location *loc2)
{
  return (loc1->reg == loc2->reg
	  && loc1->offset == loc2->offset
	  && loc1->indirect == loc2->indirect
	  && (loc1->indirect == 0
	      || loc1->base_offset == loc2->base_offset));
}

/* This routine does the actual work.  The CFA is now calculated from
   the dw_cfa_location structure.  */

static void
def_cfa_1 (dw_cfa_location *loc_p)
{
  dw_cfi_ref cfi;
  dw_cfa_location loc;

  cfa = *loc_p;
  loc = *loc_p;

  if (cfa_store.reg == loc.reg && loc.indirect == 0)
    cfa_store.offset = loc.offset;

  /* If nothing changed, no need to issue any call frame instructions.  */
  if (cfa_equal_p (&loc, &old_cfa))
    return;

  cfi = new_cfi ();

  if (loc.reg == old_cfa.reg && !loc.indirect && !old_cfa.indirect)
    {
      /* Construct a "DW_CFA_def_cfa_offset <offset>" instruction, indicating
	 the CFA register did not change but the offset did.  The data
	 factoring for DW_CFA_def_cfa_offset_sf happens in output_cfi, or
	 in the assembler via the .cfi_def_cfa_offset directive.  */
      if (loc.offset < 0)
	cfi->dw_cfi_opc = DW_CFA_def_cfa_offset_sf;
      else
	cfi->dw_cfi_opc = DW_CFA_def_cfa_offset;
      cfi->dw_cfi_oprnd1.dw_cfi_offset = loc.offset;
    }

#ifndef MIPS_DEBUGGING_INFO  /* SGI dbx thinks this means no offset.  */
  else if (loc.offset == old_cfa.offset
	   && old_cfa.reg != INVALID_REGNUM
	   && !loc.indirect
	   && !old_cfa.indirect)
    {
      /* Construct a "DW_CFA_def_cfa_register <register>" instruction,
	 indicating the CFA register has changed to <register> but the
	 offset has not changed.  */
      cfi->dw_cfi_opc = DW_CFA_def_cfa_register;
      cfi->dw_cfi_oprnd1.dw_cfi_reg_num = loc.reg;
    }
#endif

  else if (loc.indirect == 0)
    {
      /* Construct a "DW_CFA_def_cfa <register> <offset>" instruction,
	 indicating the CFA register has changed to <register> with
	 the specified offset.  The data factoring for DW_CFA_def_cfa_sf
	 happens in output_cfi, or in the assembler via the .cfi_def_cfa
	 directive.  */
      if (loc.offset < 0)
	cfi->dw_cfi_opc = DW_CFA_def_cfa_sf;
      else
	cfi->dw_cfi_opc = DW_CFA_def_cfa;
      cfi->dw_cfi_oprnd1.dw_cfi_reg_num = loc.reg;
      cfi->dw_cfi_oprnd2.dw_cfi_offset = loc.offset;
    }
  else
    {
      /* Construct a DW_CFA_def_cfa_expression instruction to
	 calculate the CFA using a full location expression since no
	 register-offset pair is available.  */
      struct dw_loc_descr_struct *loc_list;

      cfi->dw_cfi_opc = DW_CFA_def_cfa_expression;
      loc_list = build_cfa_loc (&loc, 0);
      cfi->dw_cfi_oprnd1.dw_cfi_loc = loc_list;
    }

  add_cfi (cfi);
  old_cfa = loc;
}

/* Add the CFI for saving a register.  REG is the CFA column number.
   If SREG is -1, the register is saved at OFFSET from the CFA;
   otherwise it is saved in SREG.  */

static void
reg_save (unsigned int reg, unsigned int sreg, HOST_WIDE_INT offset)
{
  dw_fde_ref fde = cfun ? cfun->fde : NULL;
  dw_cfi_ref cfi = new_cfi ();

  cfi->dw_cfi_oprnd1.dw_cfi_reg_num = reg;

  /* When stack is aligned, store REG using DW_CFA_expression with FP.  */
  if (fde
      && fde->stack_realign
      && sreg == INVALID_REGNUM)
    {
      cfi->dw_cfi_opc = DW_CFA_expression;
      cfi->dw_cfi_oprnd1.dw_cfi_reg_num = reg;
      cfi->dw_cfi_oprnd2.dw_cfi_loc
	= build_cfa_aligned_loc (&cfa, offset, fde->stack_realignment);
    }
  else if (sreg == INVALID_REGNUM)
    {
      if (need_data_align_sf_opcode (offset))
	cfi->dw_cfi_opc = DW_CFA_offset_extended_sf;
      else if (reg & ~0x3f)
	cfi->dw_cfi_opc = DW_CFA_offset_extended;
      else
	cfi->dw_cfi_opc = DW_CFA_offset;
      cfi->dw_cfi_oprnd2.dw_cfi_offset = offset;
    }
  else if (sreg == reg)
    cfi->dw_cfi_opc = DW_CFA_same_value;
  else
    {
      cfi->dw_cfi_opc = DW_CFA_register;
      cfi->dw_cfi_oprnd2.dw_cfi_reg_num = sreg;
    }

  add_cfi (cfi);
}

/* Given a SET, calculate the amount of stack adjustment it
   contains.  */

static HOST_WIDE_INT
stack_adjust_offset (const_rtx pattern, HOST_WIDE_INT cur_args_size,
		     HOST_WIDE_INT cur_offset)
{
  const_rtx src = SET_SRC (pattern);
  const_rtx dest = SET_DEST (pattern);
  HOST_WIDE_INT offset = 0;
  enum rtx_code code;

  if (dest == stack_pointer_rtx)
    {
      code = GET_CODE (src);

      /* Assume (set (reg sp) (reg whatever)) sets args_size
	 level to 0.  */
      if (code == REG && src != stack_pointer_rtx)
	{
	  offset = -cur_args_size;
#ifndef STACK_GROWS_DOWNWARD
	  offset = -offset;
#endif
	  return offset - cur_offset;
	}

      if (! (code == PLUS || code == MINUS)
	  || XEXP (src, 0) != stack_pointer_rtx
	  || !CONST_INT_P (XEXP (src, 1)))
	return 0;

      /* (set (reg sp) (plus (reg sp) (const_int))) */
      offset = INTVAL (XEXP (src, 1));
      if (code == PLUS)
	offset = -offset;
      return offset;
    }

  if (MEM_P (src) && !MEM_P (dest))
    dest = src;
  if (MEM_P (dest))
    {
      /* (set (mem (pre_dec (reg sp))) (foo)) */
      src = XEXP (dest, 0);
      code = GET_CODE (src);

      switch (code)
	{
	case PRE_MODIFY:
	case POST_MODIFY:
	  if (XEXP (src, 0) == stack_pointer_rtx)
	    {
	      rtx val = XEXP (XEXP (src, 1), 1);
	      /* We handle only adjustments by constant amount.  */
	      gcc_assert (GET_CODE (XEXP (src, 1)) == PLUS
			  && CONST_INT_P (val));
	      offset = -INTVAL (val);
	      break;
	    }
	  return 0;

	case PRE_DEC:
	case POST_DEC:
	  if (XEXP (src, 0) == stack_pointer_rtx)
	    {
	      offset = GET_MODE_SIZE (GET_MODE (dest));
	      break;
	    }
	  return 0;

	case PRE_INC:
	case POST_INC:
	  if (XEXP (src, 0) == stack_pointer_rtx)
	    {
	      offset = -GET_MODE_SIZE (GET_MODE (dest));
	      break;
	    }
	  return 0;

	default:
	  return 0;
	}
    }
  else
    return 0;

  return offset;
}

/* Precomputed args_size for CODE_LABELs and BARRIERs preceeding them,
   indexed by INSN_UID.  */

static HOST_WIDE_INT *barrier_args_size;

/* Helper function for compute_barrier_args_size.  Handle one insn.  */

static HOST_WIDE_INT
compute_barrier_args_size_1 (rtx insn, HOST_WIDE_INT cur_args_size,
			     VEC (rtx, heap) **next)
{
  HOST_WIDE_INT offset = 0;
  int i;

  if (! RTX_FRAME_RELATED_P (insn))
    {
      if (prologue_epilogue_contains (insn))
	/* Nothing */;
      else if (GET_CODE (PATTERN (insn)) == SET)
	offset = stack_adjust_offset (PATTERN (insn), cur_args_size, 0);
      else if (GET_CODE (PATTERN (insn)) == PARALLEL
	       || GET_CODE (PATTERN (insn)) == SEQUENCE)
	{
	  /* There may be stack adjustments inside compound insns.  Search
	     for them.  */
	  for (i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)
	    if (GET_CODE (XVECEXP (PATTERN (insn), 0, i)) == SET)
	      offset += stack_adjust_offset (XVECEXP (PATTERN (insn), 0, i),
					     cur_args_size, offset);
	}
    }
  else
    {
      rtx expr = find_reg_note (insn, REG_FRAME_RELATED_EXPR, NULL_RTX);

      if (expr)
	{
	  expr = XEXP (expr, 0);
	  if (GET_CODE (expr) == PARALLEL
	      || GET_CODE (expr) == SEQUENCE)
	    for (i = 1; i < XVECLEN (expr, 0); i++)
	      {
		rtx elem = XVECEXP (expr, 0, i);

		if (GET_CODE (elem) == SET && !RTX_FRAME_RELATED_P (elem))
		  offset += stack_adjust_offset (elem, cur_args_size, offset);
	      }
	}
    }

#ifndef STACK_GROWS_DOWNWARD
  offset = -offset;
#endif

  cur_args_size += offset;
  if (cur_args_size < 0)
    cur_args_size = 0;

  if (JUMP_P (insn))
    {
      rtx dest = JUMP_LABEL (insn);

      if (dest)
	{
	  if (barrier_args_size [INSN_UID (dest)] < 0)
	    {
	      barrier_args_size [INSN_UID (dest)] = cur_args_size;
	      VEC_safe_push (rtx, heap, *next, dest);
	    }
	}
    }

  return cur_args_size;
}

/* Walk the whole function and compute args_size on BARRIERs.  */

static void
compute_barrier_args_size (void)
{
  int max_uid = get_max_uid (), i;
  rtx insn;
  VEC (rtx, heap) *worklist, *next, *tmp;

  barrier_args_size = XNEWVEC (HOST_WIDE_INT, max_uid);
  for (i = 0; i < max_uid; i++)
    barrier_args_size[i] = -1;

  worklist = VEC_alloc (rtx, heap, 20);
  next = VEC_alloc (rtx, heap, 20);
  insn = get_insns ();
  barrier_args_size[INSN_UID (insn)] = 0;
  VEC_quick_push (rtx, worklist, insn);
  for (;;)
    {
      while (!VEC_empty (rtx, worklist))
	{
	  rtx prev, body, first_insn;
	  HOST_WIDE_INT cur_args_size;

	  first_insn = insn = VEC_pop (rtx, worklist);
	  cur_args_size = barrier_args_size[INSN_UID (insn)];
	  prev = prev_nonnote_insn (insn);
	  if (prev && BARRIER_P (prev))
	    barrier_args_size[INSN_UID (prev)] = cur_args_size;

	  for (; insn; insn = NEXT_INSN (insn))
	    {
	      if (INSN_DELETED_P (insn) || NOTE_P (insn))
		continue;
	      if (BARRIER_P (insn))
		break;

	      if (LABEL_P (insn))
		{
		  if (insn == first_insn)
		    continue;
		  else if (barrier_args_size[INSN_UID (insn)] < 0)
		    {
		      barrier_args_size[INSN_UID (insn)] = cur_args_size;
		      continue;
		    }
		  else
		    {
		      /* The insns starting with this label have been
			 already scanned or are in the worklist.  */
		      break;
		    }
		}

	      body = PATTERN (insn);
	      if (GET_CODE (body) == SEQUENCE)
		{
		  HOST_WIDE_INT dest_args_size = cur_args_size;
		  for (i = 1; i < XVECLEN (body, 0); i++)
		    if (INSN_ANNULLED_BRANCH_P (XVECEXP (body, 0, 0))
			&& INSN_FROM_TARGET_P (XVECEXP (body, 0, i)))
		      dest_args_size
			= compute_barrier_args_size_1 (XVECEXP (body, 0, i),
						       dest_args_size, &next);
		    else
		      cur_args_size
			= compute_barrier_args_size_1 (XVECEXP (body, 0, i),
						       cur_args_size, &next);

		  if (INSN_ANNULLED_BRANCH_P (XVECEXP (body, 0, 0)))
		    compute_barrier_args_size_1 (XVECEXP (body, 0, 0),
						 dest_args_size, &next);
		  else
		    cur_args_size
		      = compute_barrier_args_size_1 (XVECEXP (body, 0, 0),
						     cur_args_size, &next);
		}
	      else
		cur_args_size
		  = compute_barrier_args_size_1 (insn, cur_args_size, &next);
	    }
	}

      if (VEC_empty (rtx, next))
	break;

      /* Swap WORKLIST with NEXT and truncate NEXT for next iteration.  */
      tmp = next;
      next = worklist;
      worklist = tmp;
      VEC_truncate (rtx, next, 0);
    }

  VEC_free (rtx, heap, worklist);
  VEC_free (rtx, heap, next);
}

/* Add a CFI to update the running total of the size of arguments
   pushed onto the stack.  */

static void
dwarf2out_args_size (HOST_WIDE_INT size)
{
  dw_cfi_ref cfi;

  if (size == old_args_size)
    return;

  old_args_size = size;

  cfi = new_cfi ();
  cfi->dw_cfi_opc = DW_CFA_GNU_args_size;
  cfi->dw_cfi_oprnd1.dw_cfi_offset = size;
  add_cfi (cfi);
}

/* Record a stack adjustment of OFFSET bytes.  */

static void
dwarf2out_stack_adjust (HOST_WIDE_INT offset)
{
  if (cfa.reg == DW_STACK_POINTER_REGNUM)
    cfa.offset += offset;

  if (cfa_store.reg == DW_STACK_POINTER_REGNUM)
    cfa_store.offset += offset;

  if (ACCUMULATE_OUTGOING_ARGS)
    return;

#ifndef STACK_GROWS_DOWNWARD
  offset = -offset;
#endif

  args_size += offset;
  if (args_size < 0)
    args_size = 0;

  def_cfa_1 (&cfa);
  if (flag_asynchronous_unwind_tables)
    dwarf2out_args_size (args_size);
}

/* Check INSN to see if it looks like a push or a stack adjustment, and
   make a note of it if it does.  EH uses this information to find out
   how much extra space it needs to pop off the stack.  */

static void
dwarf2out_notice_stack_adjust (rtx insn, bool after_p)
{
  HOST_WIDE_INT offset;
  int i;

  /* Don't handle epilogues at all.  Certainly it would be wrong to do so
     with this function.  Proper support would require all frame-related
     insns to be marked, and to be able to handle saving state around
     epilogues textually in the middle of the function.  */
  if (prologue_epilogue_contains (insn))
    return;

  /* If INSN is an instruction from target of an annulled branch, the
     effects are for the target only and so current argument size
     shouldn't change at all.  */
  if (final_sequence
      && INSN_ANNULLED_BRANCH_P (XVECEXP (final_sequence, 0, 0))
      && INSN_FROM_TARGET_P (insn))
    return;

  /* If only calls can throw, and we have a frame pointer,
     save up adjustments until we see the CALL_INSN.  */
  if (!flag_asynchronous_unwind_tables && cfa.reg != DW_STACK_POINTER_REGNUM)
    {
      if (CALL_P (insn) && !after_p)
	{
	  /* Extract the size of the args from the CALL rtx itself.  */
	  insn = PATTERN (insn);
	  if (GET_CODE (insn) == PARALLEL)
	    insn = XVECEXP (insn, 0, 0);
	  if (GET_CODE (insn) == SET)
	    insn = SET_SRC (insn);
	  gcc_assert (GET_CODE (insn) == CALL);
	  dwarf2out_args_size (INTVAL (XEXP (insn, 1)));
	}
      return;
    }

  if (CALL_P (insn) && !after_p)
    {
      if (!flag_asynchronous_unwind_tables)
	dwarf2out_args_size (args_size);
      return;
    }
  else if (BARRIER_P (insn))
    {
      /* Don't call compute_barrier_args_size () if the only
	 BARRIER is at the end of function.  */
      if (barrier_args_size == NULL && next_nonnote_insn (insn))
	compute_barrier_args_size ();
      if (barrier_args_size == NULL)
	offset = 0;
      else
	{
	  offset = barrier_args_size[INSN_UID (insn)];
	  if (offset < 0)
	    offset = 0;
	}

      offset -= args_size;
#ifndef STACK_GROWS_DOWNWARD
      offset = -offset;
#endif
    }
  else if (GET_CODE (PATTERN (insn)) == SET)
    offset = stack_adjust_offset (PATTERN (insn), args_size, 0);
  else if (GET_CODE (PATTERN (insn)) == PARALLEL
	   || GET_CODE (PATTERN (insn)) == SEQUENCE)
    {
      /* There may be stack adjustments inside compound insns.  Search
	 for them.  */
      for (offset = 0, i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)
	if (GET_CODE (XVECEXP (PATTERN (insn), 0, i)) == SET)
	  offset += stack_adjust_offset (XVECEXP (PATTERN (insn), 0, i),
					 args_size, offset);
    }
  else
    return;

  if (offset == 0)
    return;

  dwarf2out_stack_adjust (offset);
}

/* We delay emitting a register save until either (a) we reach the end
   of the prologue or (b) the register is clobbered.  This clusters
   register saves so that there are fewer pc advances.  */

struct GTY(()) queued_reg_save {
  struct queued_reg_save *next;
  rtx reg;
  HOST_WIDE_INT cfa_offset;
  rtx saved_reg;
};

static GTY(()) struct queued_reg_save *queued_reg_saves;

/* The caller's ORIG_REG is saved in SAVED_IN_REG.  */
typedef struct GTY(()) reg_saved_in_data {
  rtx orig_reg;
  rtx saved_in_reg;
} reg_saved_in_data;

DEF_VEC_O (reg_saved_in_data);
DEF_VEC_ALLOC_O (reg_saved_in_data, gc);

/* A set of registers saved in other registers.  This is implemented as
   a flat array because it normally contains zero or 1 entry, depending
   on the target.  IA-64 is the big spender here, using a maximum of
   5 entries.  */
static GTY(()) VEC(reg_saved_in_data, gc) *regs_saved_in_regs;

static GTY(()) reg_saved_in_data *cie_return_save;

/* Short-hand inline for the very common D_F_R (REGNO (x)) operation.  */
/* ??? This ought to go into dwarf2out.h alongside dwarf_frame_regnum,
   except that dwarf2out.h is used in places where rtl is prohibited.  */

static inline unsigned
dwf_regno (const_rtx reg)
{
  return dwarf_frame_regnum (REGNO (reg));
}

/* Compare X and Y for equivalence.  The inputs may be REGs or PC_RTX.  */

static bool
compare_reg_or_pc (rtx x, rtx y)
{
  if (REG_P (x) && REG_P (y))
    return REGNO (x) == REGNO (y);
  return x == y;
}

/* Record SRC as being saved in DEST.  DEST may be null to delete an
   existing entry.  SRC may be a register or PC_RTX.  */

static void
record_reg_saved_in_reg (rtx dest, rtx src)
{
  reg_saved_in_data *elt;
  size_t i;

  FOR_EACH_VEC_ELT (reg_saved_in_data, regs_saved_in_regs, i, elt)
    if (compare_reg_or_pc (elt->orig_reg, src))
      {
	if (dest == NULL)
	  VEC_unordered_remove(reg_saved_in_data, regs_saved_in_regs, i);
	else
	  elt->saved_in_reg = dest;
	return;
      }

  if (dest == NULL)
    return;

  elt = VEC_safe_push(reg_saved_in_data, gc, regs_saved_in_regs, NULL);
  elt->orig_reg = src;
  elt->saved_in_reg = dest;
}

/* Add an entry to QUEUED_REG_SAVES saying that REG is now saved at
   SREG, or if SREG is NULL then it is saved at OFFSET to the CFA.  */

static void
queue_reg_save (rtx reg, rtx sreg, HOST_WIDE_INT offset)
{
  struct queued_reg_save *q;

  /* Duplicates waste space, but it's also necessary to remove them
     for correctness, since the queue gets output in reverse order.  */
  for (q = queued_reg_saves; q != NULL; q = q->next)
    if (compare_reg_or_pc (q->reg, reg))
      break;

  if (q == NULL)
    {
      q = ggc_alloc_queued_reg_save ();
      q->next = queued_reg_saves;
      queued_reg_saves = q;
    }

  q->reg = reg;
  q->cfa_offset = offset;
  q->saved_reg = sreg;
}

/* Output all the entries in QUEUED_REG_SAVES.  */

static void
dwarf2out_flush_queued_reg_saves (void)
{
  struct queued_reg_save *q;

  for (q = queued_reg_saves; q; q = q->next)
    {
      unsigned int reg, sreg;

      record_reg_saved_in_reg (q->saved_reg, q->reg);

      if (q->reg == pc_rtx)
	reg = DWARF_FRAME_RETURN_COLUMN;
      else
        reg = dwf_regno (q->reg);
      if (q->saved_reg)
	sreg = dwf_regno (q->saved_reg);
      else
	sreg = INVALID_REGNUM;
      reg_save (reg, sreg, q->cfa_offset);
    }

  queued_reg_saves = NULL;
}

/* Does INSN clobber any register which QUEUED_REG_SAVES lists a saved
   location for?  Or, does it clobber a register which we've previously
   said that some other register is saved in, and for which we now
   have a new location for?  */

static bool
clobbers_queued_reg_save (const_rtx insn)
{
  struct queued_reg_save *q;

  for (q = queued_reg_saves; q; q = q->next)
    {
      size_t i;
      reg_saved_in_data *rir;

      if (modified_in_p (q->reg, insn))
	return true;

      FOR_EACH_VEC_ELT (reg_saved_in_data, regs_saved_in_regs, i, rir)
	if (compare_reg_or_pc (q->reg, rir->orig_reg)
	    && modified_in_p (rir->saved_in_reg, insn))
	  return true;
    }

  return false;
}

/* What register, if any, is currently saved in REG?  */

static rtx
reg_saved_in (rtx reg)
{
  unsigned int regn = REGNO (reg);
  struct queued_reg_save *q;
  reg_saved_in_data *rir;
  size_t i;

  for (q = queued_reg_saves; q; q = q->next)
    if (q->saved_reg && regn == REGNO (q->saved_reg))
      return q->reg;

  FOR_EACH_VEC_ELT (reg_saved_in_data, regs_saved_in_regs, i, rir)
    if (regn == REGNO (rir->saved_in_reg))
      return rir->orig_reg;

  return NULL_RTX;
}


/* A temporary register holding an integral value used in adjusting SP
   or setting up the store_reg.  The "offset" field holds the integer
   value, not an offset.  */
static dw_cfa_location cfa_temp;

/* A subroutine of dwarf2out_frame_debug, process a REG_DEF_CFA note.  */

static void
dwarf2out_frame_debug_def_cfa (rtx pat)
{
  memset (&cfa, 0, sizeof (cfa));

  switch (GET_CODE (pat))
    {
    case PLUS:
      cfa.reg = dwf_regno (XEXP (pat, 0));
      cfa.offset = INTVAL (XEXP (pat, 1));
      break;

    case REG:
      cfa.reg = dwf_regno (pat);
      break;

    case MEM:
      cfa.indirect = 1;
      pat = XEXP (pat, 0);
      if (GET_CODE (pat) == PLUS)
	{
	  cfa.base_offset = INTVAL (XEXP (pat, 1));
	  pat = XEXP (pat, 0);
	}
      cfa.reg = dwf_regno (pat);
      break;

    default:
      /* Recurse and define an expression.  */
      gcc_unreachable ();
    }

  def_cfa_1 (&cfa);
}

/* A subroutine of dwarf2out_frame_debug, process a REG_ADJUST_CFA note.  */

static void
dwarf2out_frame_debug_adjust_cfa (rtx pat)
{
  rtx src, dest;

  gcc_assert (GET_CODE (pat) == SET);
  dest = XEXP (pat, 0);
  src = XEXP (pat, 1);

  switch (GET_CODE (src))
    {
    case PLUS:
      gcc_assert (dwf_regno (XEXP (src, 0)) == cfa.reg);
      cfa.offset -= INTVAL (XEXP (src, 1));
      break;

    case REG:
	break;

    default:
	gcc_unreachable ();
    }

  cfa.reg = dwf_regno (dest);
  gcc_assert (cfa.indirect == 0);

  def_cfa_1 (&cfa);
}

/* A subroutine of dwarf2out_frame_debug, process a REG_CFA_OFFSET note.  */

static void
dwarf2out_frame_debug_cfa_offset (rtx set)
{
  HOST_WIDE_INT offset;
  rtx src, addr, span;
  unsigned int sregno;

  src = XEXP (set, 1);
  addr = XEXP (set, 0);
  gcc_assert (MEM_P (addr));
  addr = XEXP (addr, 0);

  /* As documented, only consider extremely simple addresses.  */
  switch (GET_CODE (addr))
    {
    case REG:
      gcc_assert (dwf_regno (addr) == cfa.reg);
      offset = -cfa.offset;
      break;
    case PLUS:
      gcc_assert (dwf_regno (XEXP (addr, 0)) == cfa.reg);
      offset = INTVAL (XEXP (addr, 1)) - cfa.offset;
      break;
    default:
      gcc_unreachable ();
    }

  if (src == pc_rtx)
    {
      span = NULL;
      sregno = DWARF_FRAME_RETURN_COLUMN;
    }
  else 
    {
      span = targetm.dwarf_register_span (src);
      sregno = dwf_regno (src);
    }

  /* ??? We'd like to use queue_reg_save, but we need to come up with
     a different flushing heuristic for epilogues.  */
  if (!span)
    reg_save (sregno, INVALID_REGNUM, offset);
  else
    {
      /* We have a PARALLEL describing where the contents of SRC live.
   	 Queue register saves for each piece of the PARALLEL.  */
      int par_index;
      int limit;
      HOST_WIDE_INT span_offset = offset;

      gcc_assert (GET_CODE (span) == PARALLEL);

      limit = XVECLEN (span, 0);
      for (par_index = 0; par_index < limit; par_index++)
	{
	  rtx elem = XVECEXP (span, 0, par_index);

	  sregno = dwf_regno (src);
	  reg_save (sregno, INVALID_REGNUM, span_offset);
	  span_offset += GET_MODE_SIZE (GET_MODE (elem));
	}
    }
}

/* A subroutine of dwarf2out_frame_debug, process a REG_CFA_REGISTER note.  */

static void
dwarf2out_frame_debug_cfa_register (rtx set)
{
  rtx src, dest;
  unsigned sregno, dregno;

  src = XEXP (set, 1);
  dest = XEXP (set, 0);

  record_reg_saved_in_reg (dest, src);
  if (src == pc_rtx)
    sregno = DWARF_FRAME_RETURN_COLUMN;
  else
    sregno = dwf_regno (src);

  dregno = dwf_regno (dest);

  /* ??? We'd like to use queue_reg_save, but we need to come up with
     a different flushing heuristic for epilogues.  */
  reg_save (sregno, dregno, 0);
}

/* A subroutine of dwarf2out_frame_debug, process a REG_CFA_EXPRESSION note. */

static void
dwarf2out_frame_debug_cfa_expression (rtx set)
{
  rtx src, dest, span;
  dw_cfi_ref cfi = new_cfi ();

  dest = SET_DEST (set);
  src = SET_SRC (set);

  gcc_assert (REG_P (src));
  gcc_assert (MEM_P (dest));

  span = targetm.dwarf_register_span (src);
  gcc_assert (!span);

  cfi->dw_cfi_opc = DW_CFA_expression;
  cfi->dw_cfi_oprnd1.dw_cfi_reg_num = dwf_regno (src);
  cfi->dw_cfi_oprnd2.dw_cfi_loc
    = mem_loc_descriptor (XEXP (dest, 0), get_address_mode (dest),
			  GET_MODE (dest), VAR_INIT_STATUS_INITIALIZED);

  /* ??? We'd like to use queue_reg_save, were the interface different,
     and, as above, we could manage flushing for epilogues.  */
  add_cfi (cfi);
}

/* A subroutine of dwarf2out_frame_debug, process a REG_CFA_RESTORE note.  */

static void
dwarf2out_frame_debug_cfa_restore (rtx reg)
{
  dw_cfi_ref cfi = new_cfi ();
  unsigned int regno = dwf_regno (reg);

  cfi->dw_cfi_opc = (regno & ~0x3f ? DW_CFA_restore_extended : DW_CFA_restore);
  cfi->dw_cfi_oprnd1.dw_cfi_reg_num = regno;

  add_cfi (cfi);
}

/* A subroutine of dwarf2out_frame_debug, process a REG_CFA_WINDOW_SAVE.
   ??? Perhaps we should note in the CIE where windows are saved (instead of
   assuming 0(cfa)) and what registers are in the window.  */

static void
dwarf2out_frame_debug_cfa_window_save (void)
{
  dw_cfi_ref cfi = new_cfi ();

  cfi->dw_cfi_opc = DW_CFA_GNU_window_save;
  add_cfi (cfi);
}

/* Record call frame debugging information for an expression EXPR,
   which either sets SP or FP (adjusting how we calculate the frame
   address) or saves a register to the stack or another register.
   LABEL indicates the address of EXPR.

   This function encodes a state machine mapping rtxes to actions on
   cfa, cfa_store, and cfa_temp.reg.  We describe these rules so
   users need not read the source code.

  The High-Level Picture

  Changes in the register we use to calculate the CFA: Currently we
  assume that if you copy the CFA register into another register, we
  should take the other one as the new CFA register; this seems to
  work pretty well.  If it's wrong for some target, it's simple
  enough not to set RTX_FRAME_RELATED_P on the insn in question.

  Changes in the register we use for saving registers to the stack:
  This is usually SP, but not always.  Again, we deduce that if you
  copy SP into another register (and SP is not the CFA register),
  then the new register is the one we will be using for register
  saves.  This also seems to work.

  Register saves: There's not much guesswork about this one; if
  RTX_FRAME_RELATED_P is set on an insn which modifies memory, it's a
  register save, and the register used to calculate the destination
  had better be the one we think we're using for this purpose.
  It's also assumed that a copy from a call-saved register to another
  register is saving that register if RTX_FRAME_RELATED_P is set on
  that instruction.  If the copy is from a call-saved register to
  the *same* register, that means that the register is now the same
  value as in the caller.

  Except: If the register being saved is the CFA register, and the
  offset is nonzero, we are saving the CFA, so we assume we have to
  use DW_CFA_def_cfa_expression.  If the offset is 0, we assume that
  the intent is to save the value of SP from the previous frame.

  In addition, if a register has previously been saved to a different
  register,

  Invariants / Summaries of Rules

  cfa	       current rule for calculating the CFA.  It usually
	       consists of a register and an offset.
  cfa_store    register used by prologue code to save things to the stack
	       cfa_store.offset is the offset from the value of
	       cfa_store.reg to the actual CFA
  cfa_temp     register holding an integral value.  cfa_temp.offset
	       stores the value, which will be used to adjust the
	       stack pointer.  cfa_temp is also used like cfa_store,
	       to track stores to the stack via fp or a temp reg.

  Rules  1- 4: Setting a register's value to cfa.reg or an expression
	       with cfa.reg as the first operand changes the cfa.reg and its
	       cfa.offset.  Rule 1 and 4 also set cfa_temp.reg and
	       cfa_temp.offset.

  Rules  6- 9: Set a non-cfa.reg register value to a constant or an
	       expression yielding a constant.  This sets cfa_temp.reg
	       and cfa_temp.offset.

  Rule 5:      Create a new register cfa_store used to save items to the
	       stack.

  Rules 10-14: Save a register to the stack.  Define offset as the
	       difference of the original location and cfa_store's
	       location (or cfa_temp's location if cfa_temp is used).

  Rules 16-20: If AND operation happens on sp in prologue, we assume
	       stack is realigned.  We will use a group of DW_OP_XXX
	       expressions to represent the location of the stored
	       register instead of CFA+offset.

  The Rules

  "{a,b}" indicates a choice of a xor b.
  "<reg>:cfa.reg" indicates that <reg> must equal cfa.reg.

  Rule 1:
  (set <reg1> <reg2>:cfa.reg)
  effects: cfa.reg = <reg1>
	   cfa.offset unchanged
	   cfa_temp.reg = <reg1>
	   cfa_temp.offset = cfa.offset

  Rule 2:
  (set sp ({minus,plus,losum} {sp,fp}:cfa.reg
			      {<const_int>,<reg>:cfa_temp.reg}))
  effects: cfa.reg = sp if fp used
	   cfa.offset += {+/- <const_int>, cfa_temp.offset} if cfa.reg==sp
	   cfa_store.offset += {+/- <const_int>, cfa_temp.offset}
	     if cfa_store.reg==sp

  Rule 3:
  (set fp ({minus,plus,losum} <reg>:cfa.reg <const_int>))
  effects: cfa.reg = fp
	   cfa_offset += +/- <const_int>

  Rule 4:
  (set <reg1> ({plus,losum} <reg2>:cfa.reg <const_int>))
  constraints: <reg1> != fp
	       <reg1> != sp
  effects: cfa.reg = <reg1>
	   cfa_temp.reg = <reg1>
	   cfa_temp.offset = cfa.offset

  Rule 5:
  (set <reg1> (plus <reg2>:cfa_temp.reg sp:cfa.reg))
  constraints: <reg1> != fp
	       <reg1> != sp
  effects: cfa_store.reg = <reg1>
	   cfa_store.offset = cfa.offset - cfa_temp.offset

  Rule 6:
  (set <reg> <const_int>)
  effects: cfa_temp.reg = <reg>
	   cfa_temp.offset = <const_int>

  Rule 7:
  (set <reg1>:cfa_temp.reg (ior <reg2>:cfa_temp.reg <const_int>))
  effects: cfa_temp.reg = <reg1>
	   cfa_temp.offset |= <const_int>

  Rule 8:
  (set <reg> (high <exp>))
  effects: none

  Rule 9:
  (set <reg> (lo_sum <exp> <const_int>))
  effects: cfa_temp.reg = <reg>
	   cfa_temp.offset = <const_int>

  Rule 10:
  (set (mem ({pre,post}_modify sp:cfa_store (???? <reg1> <const_int>))) <reg2>)
  effects: cfa_store.offset -= <const_int>
	   cfa.offset = cfa_store.offset if cfa.reg == sp
	   cfa.reg = sp
	   cfa.base_offset = -cfa_store.offset

  Rule 11:
  (set (mem ({pre_inc,pre_dec,post_dec} sp:cfa_store.reg)) <reg>)
  effects: cfa_store.offset += -/+ mode_size(mem)
	   cfa.offset = cfa_store.offset if cfa.reg == sp
	   cfa.reg = sp
	   cfa.base_offset = -cfa_store.offset

  Rule 12:
  (set (mem ({minus,plus,losum} <reg1>:{cfa_store,cfa_temp} <const_int>))

       <reg2>)
  effects: cfa.reg = <reg1>
	   cfa.base_offset = -/+ <const_int> - {cfa_store,cfa_temp}.offset

  Rule 13:
  (set (mem <reg1>:{cfa_store,cfa_temp}) <reg2>)
  effects: cfa.reg = <reg1>
	   cfa.base_offset = -{cfa_store,cfa_temp}.offset

  Rule 14:
  (set (mem (post_inc <reg1>:cfa_temp <const_int>)) <reg2>)
  effects: cfa.reg = <reg1>
	   cfa.base_offset = -cfa_temp.offset
	   cfa_temp.offset -= mode_size(mem)

  Rule 15:
  (set <reg> {unspec, unspec_volatile})
  effects: target-dependent

  Rule 16:
  (set sp (and: sp <const_int>))
  constraints: cfa_store.reg == sp
  effects: cfun->fde.stack_realign = 1
           cfa_store.offset = 0
	   fde->drap_reg = cfa.reg if cfa.reg != sp and cfa.reg != fp

  Rule 17:
  (set (mem ({pre_inc, pre_dec} sp)) (mem (plus (cfa.reg) (const_int))))
  effects: cfa_store.offset += -/+ mode_size(mem)

  Rule 18:
  (set (mem ({pre_inc, pre_dec} sp)) fp)
  constraints: fde->stack_realign == 1
  effects: cfa_store.offset = 0
	   cfa.reg != HARD_FRAME_POINTER_REGNUM

  Rule 19:
  (set (mem ({pre_inc, pre_dec} sp)) cfa.reg)
  constraints: fde->stack_realign == 1
               && cfa.offset == 0
               && cfa.indirect == 0
               && cfa.reg != HARD_FRAME_POINTER_REGNUM
  effects: Use DW_CFA_def_cfa_expression to define cfa
  	   cfa.reg == fde->drap_reg  */

static void
dwarf2out_frame_debug_expr (rtx expr)
{
  rtx src, dest, span;
  HOST_WIDE_INT offset;
  dw_fde_ref fde;

  /* If RTX_FRAME_RELATED_P is set on a PARALLEL, process each member of
     the PARALLEL independently. The first element is always processed if
     it is a SET. This is for backward compatibility.   Other elements
     are processed only if they are SETs and the RTX_FRAME_RELATED_P
     flag is set in them.  */
  if (GET_CODE (expr) == PARALLEL || GET_CODE (expr) == SEQUENCE)
    {
      int par_index;
      int limit = XVECLEN (expr, 0);
      rtx elem;

      /* PARALLELs have strict read-modify-write semantics, so we
	 ought to evaluate every rvalue before changing any lvalue.
	 It's cumbersome to do that in general, but there's an
	 easy approximation that is enough for all current users:
	 handle register saves before register assignments.  */
      if (GET_CODE (expr) == PARALLEL)
	for (par_index = 0; par_index < limit; par_index++)
	  {
	    elem = XVECEXP (expr, 0, par_index);
	    if (GET_CODE (elem) == SET
		&& MEM_P (SET_DEST (elem))
		&& (RTX_FRAME_RELATED_P (elem) || par_index == 0))
	      dwarf2out_frame_debug_expr (elem);
	  }

      for (par_index = 0; par_index < limit; par_index++)
	{
	  elem = XVECEXP (expr, 0, par_index);
	  if (GET_CODE (elem) == SET
	      && (!MEM_P (SET_DEST (elem)) || GET_CODE (expr) == SEQUENCE)
	      && (RTX_FRAME_RELATED_P (elem) || par_index == 0))
	    dwarf2out_frame_debug_expr (elem);
	  else if (GET_CODE (elem) == SET
		   && par_index != 0
		   && !RTX_FRAME_RELATED_P (elem))
	    {
	      /* Stack adjustment combining might combine some post-prologue
		 stack adjustment into a prologue stack adjustment.  */
	      HOST_WIDE_INT offset = stack_adjust_offset (elem, args_size, 0);

	      if (offset != 0)
		dwarf2out_stack_adjust (offset);
	    }
	}
      return;
    }

  gcc_assert (GET_CODE (expr) == SET);

  src = SET_SRC (expr);
  dest = SET_DEST (expr);

  if (REG_P (src))
    {
      rtx rsi = reg_saved_in (src);
      if (rsi)
	src = rsi;
    }

  fde = cfun->fde;

  switch (GET_CODE (dest))
    {
    case REG:
      switch (GET_CODE (src))
	{
	  /* Setting FP from SP.  */
	case REG:
	  if (cfa.reg == dwf_regno (src))
	    {
	      /* Rule 1 */
	      /* Update the CFA rule wrt SP or FP.  Make sure src is
		 relative to the current CFA register.

		 We used to require that dest be either SP or FP, but the
		 ARM copies SP to a temporary register, and from there to
		 FP.  So we just rely on the backends to only set
		 RTX_FRAME_RELATED_P on appropriate insns.  */
	      cfa.reg = dwf_regno (dest);
	      cfa_temp.reg = cfa.reg;
	      cfa_temp.offset = cfa.offset;
	    }
	  else
	    {
	      /* Saving a register in a register.  */
	      gcc_assert (!fixed_regs [REGNO (dest)]
			  /* For the SPARC and its register window.  */
			  || (dwf_regno (src) == DWARF_FRAME_RETURN_COLUMN));

              /* After stack is aligned, we can only save SP in FP
		 if drap register is used.  In this case, we have
		 to restore stack pointer with the CFA value and we
		 don't generate this DWARF information.  */
	      if (fde
		  && fde->stack_realign
		  && REGNO (src) == STACK_POINTER_REGNUM)
		gcc_assert (REGNO (dest) == HARD_FRAME_POINTER_REGNUM
			    && fde->drap_reg != INVALID_REGNUM
			    && cfa.reg != dwf_regno (src));
	      else
		queue_reg_save (src, dest, 0);
	    }
	  break;

	case PLUS:
	case MINUS:
	case LO_SUM:
	  if (dest == stack_pointer_rtx)
	    {
	      /* Rule 2 */
	      /* Adjusting SP.  */
	      switch (GET_CODE (XEXP (src, 1)))
		{
		case CONST_INT:
		  offset = INTVAL (XEXP (src, 1));
		  break;
		case REG:
		  gcc_assert (dwf_regno (XEXP (src, 1)) == cfa_temp.reg);
		  offset = cfa_temp.offset;
		  break;
		default:
		  gcc_unreachable ();
		}

	      if (XEXP (src, 0) == hard_frame_pointer_rtx)
		{
		  /* Restoring SP from FP in the epilogue.  */
		  gcc_assert (cfa.reg == DW_FRAME_POINTER_REGNUM);
		  cfa.reg = DW_STACK_POINTER_REGNUM;
		}
	      else if (GET_CODE (src) == LO_SUM)
		/* Assume we've set the source reg of the LO_SUM from sp.  */
		;
	      else
		gcc_assert (XEXP (src, 0) == stack_pointer_rtx);

	      if (GET_CODE (src) != MINUS)
		offset = -offset;
	      if (cfa.reg == DW_STACK_POINTER_REGNUM)
		cfa.offset += offset;
	      if (cfa_store.reg == DW_STACK_POINTER_REGNUM)
		cfa_store.offset += offset;
	    }
	  else if (dest == hard_frame_pointer_rtx)
	    {
	      /* Rule 3 */
	      /* Either setting the FP from an offset of the SP,
		 or adjusting the FP */
	      gcc_assert (frame_pointer_needed);

	      gcc_assert (REG_P (XEXP (src, 0))
			  && dwf_regno (XEXP (src, 0)) == cfa.reg
			  && CONST_INT_P (XEXP (src, 1)));
	      offset = INTVAL (XEXP (src, 1));
	      if (GET_CODE (src) != MINUS)
		offset = -offset;
	      cfa.offset += offset;
	      cfa.reg = DW_FRAME_POINTER_REGNUM;
	    }
	  else
	    {
	      gcc_assert (GET_CODE (src) != MINUS);

	      /* Rule 4 */
	      if (REG_P (XEXP (src, 0))
		  && dwf_regno (XEXP (src, 0)) == cfa.reg
		  && CONST_INT_P (XEXP (src, 1)))
		{
		  /* Setting a temporary CFA register that will be copied
		     into the FP later on.  */
		  offset = - INTVAL (XEXP (src, 1));
		  cfa.offset += offset;
		  cfa.reg = dwf_regno (dest);
		  /* Or used to save regs to the stack.  */
		  cfa_temp.reg = cfa.reg;
		  cfa_temp.offset = cfa.offset;
		}

	      /* Rule 5 */
	      else if (REG_P (XEXP (src, 0))
		       && dwf_regno (XEXP (src, 0)) == cfa_temp.reg
		       && XEXP (src, 1) == stack_pointer_rtx)
		{
		  /* Setting a scratch register that we will use instead
		     of SP for saving registers to the stack.  */
		  gcc_assert (cfa.reg == DW_STACK_POINTER_REGNUM);
		  cfa_store.reg = dwf_regno (dest);
		  cfa_store.offset = cfa.offset - cfa_temp.offset;
		}

	      /* Rule 9 */
	      else if (GET_CODE (src) == LO_SUM
		       && CONST_INT_P (XEXP (src, 1)))
		{
		  cfa_temp.reg = dwf_regno (dest);
		  cfa_temp.offset = INTVAL (XEXP (src, 1));
		}
	      else
		gcc_unreachable ();
	    }
	  break;

	  /* Rule 6 */
	case CONST_INT:
	  cfa_temp.reg = dwf_regno (dest);
	  cfa_temp.offset = INTVAL (src);
	  break;

	  /* Rule 7 */
	case IOR:
	  gcc_assert (REG_P (XEXP (src, 0))
		      && dwf_regno (XEXP (src, 0)) == cfa_temp.reg
		      && CONST_INT_P (XEXP (src, 1)));

	  cfa_temp.reg = dwf_regno (dest);
	  cfa_temp.offset |= INTVAL (XEXP (src, 1));
	  break;

	  /* Skip over HIGH, assuming it will be followed by a LO_SUM,
	     which will fill in all of the bits.  */
	  /* Rule 8 */
	case HIGH:
	  break;

	  /* Rule 15 */
	case UNSPEC:
	case UNSPEC_VOLATILE:
	  /* All unspecs should be represented by REG_CFA_* notes.  */
	  gcc_unreachable ();
	  return;

	  /* Rule 16 */
	case AND:
          /* If this AND operation happens on stack pointer in prologue,
	     we assume the stack is realigned and we extract the
	     alignment.  */
          if (fde && XEXP (src, 0) == stack_pointer_rtx)
            {
	      /* We interpret reg_save differently with stack_realign set.
		 Thus we must flush whatever we have queued first.  */
	      dwarf2out_flush_queued_reg_saves ();

              gcc_assert (cfa_store.reg == dwf_regno (XEXP (src, 0)));
              fde->stack_realign = 1;
              fde->stack_realignment = INTVAL (XEXP (src, 1));
              cfa_store.offset = 0;

	      if (cfa.reg != DW_STACK_POINTER_REGNUM
		  && cfa.reg != DW_FRAME_POINTER_REGNUM)
		fde->drap_reg = cfa.reg;
            }
          return;

	default:
	  gcc_unreachable ();
	}

      def_cfa_1 (&cfa);
      break;

    case MEM:

      /* Saving a register to the stack.  Make sure dest is relative to the
	 CFA register.  */
      switch (GET_CODE (XEXP (dest, 0)))
	{
	  /* Rule 10 */
	  /* With a push.  */
	case PRE_MODIFY:
	case POST_MODIFY:
	  /* We can't handle variable size modifications.  */
	  gcc_assert (GET_CODE (XEXP (XEXP (XEXP (dest, 0), 1), 1))
		      == CONST_INT);
	  offset = -INTVAL (XEXP (XEXP (XEXP (dest, 0), 1), 1));

	  gcc_assert (REGNO (XEXP (XEXP (dest, 0), 0)) == STACK_POINTER_REGNUM
		      && cfa_store.reg == DW_STACK_POINTER_REGNUM);

	  cfa_store.offset += offset;
	  if (cfa.reg == DW_STACK_POINTER_REGNUM)
	    cfa.offset = cfa_store.offset;

	  if (GET_CODE (XEXP (dest, 0)) == POST_MODIFY)
	    offset -= cfa_store.offset;
	  else
	    offset = -cfa_store.offset;
	  break;

	  /* Rule 11 */
	case PRE_INC:
	case PRE_DEC:
	case POST_DEC:
	  offset = GET_MODE_SIZE (GET_MODE (dest));
	  if (GET_CODE (XEXP (dest, 0)) == PRE_INC)
	    offset = -offset;

	  gcc_assert ((REGNO (XEXP (XEXP (dest, 0), 0))
		       == STACK_POINTER_REGNUM)
		      && cfa_store.reg == DW_STACK_POINTER_REGNUM);

	  cfa_store.offset += offset;

          /* Rule 18: If stack is aligned, we will use FP as a
	     reference to represent the address of the stored
	     regiser.  */
          if (fde
              && fde->stack_realign
              && src == hard_frame_pointer_rtx)
	    {
	      gcc_assert (cfa.reg != DW_FRAME_POINTER_REGNUM);
	      cfa_store.offset = 0;
	    }

	  if (cfa.reg == DW_STACK_POINTER_REGNUM)
	    cfa.offset = cfa_store.offset;

	  if (GET_CODE (XEXP (dest, 0)) == POST_DEC)
	    offset += -cfa_store.offset;
	  else
	    offset = -cfa_store.offset;
	  break;

	  /* Rule 12 */
	  /* With an offset.  */
	case PLUS:
	case MINUS:
	case LO_SUM:
	  {
	    unsigned int regno;

	    gcc_assert (CONST_INT_P (XEXP (XEXP (dest, 0), 1))
			&& REG_P (XEXP (XEXP (dest, 0), 0)));
	    offset = INTVAL (XEXP (XEXP (dest, 0), 1));
	    if (GET_CODE (XEXP (dest, 0)) == MINUS)
	      offset = -offset;

	    regno = dwf_regno (XEXP (XEXP (dest, 0), 0));

	    if (cfa.reg == regno)
	      offset -= cfa.offset;
	    else if (cfa_store.reg == regno)
	      offset -= cfa_store.offset;
	    else
	      {
		gcc_assert (cfa_temp.reg == regno);
		offset -= cfa_temp.offset;
	      }
	  }
	  break;

	  /* Rule 13 */
	  /* Without an offset.  */
	case REG:
	  {
	    unsigned int regno = dwf_regno (XEXP (dest, 0));

	    if (cfa.reg == regno)
	      offset = -cfa.offset;
	    else if (cfa_store.reg == regno)
	      offset = -cfa_store.offset;
	    else
	      {
		gcc_assert (cfa_temp.reg == regno);
		offset = -cfa_temp.offset;
	      }
	  }
	  break;

	  /* Rule 14 */
	case POST_INC:
	  gcc_assert (cfa_temp.reg == dwf_regno (XEXP (XEXP (dest, 0), 0)));
	  offset = -cfa_temp.offset;
	  cfa_temp.offset -= GET_MODE_SIZE (GET_MODE (dest));
	  break;

	default:
	  gcc_unreachable ();
	}

      /* Rule 17 */
      /* If the source operand of this MEM operation is a memory,
	 we only care how much stack grew.  */
      if (MEM_P (src))
        break;

      if (REG_P (src)
	  && REGNO (src) != STACK_POINTER_REGNUM
	  && REGNO (src) != HARD_FRAME_POINTER_REGNUM
	  && dwf_regno (src) == cfa.reg)
	{
	  /* We're storing the current CFA reg into the stack.  */

	  if (cfa.offset == 0)
	    {
              /* Rule 19 */
              /* If stack is aligned, putting CFA reg into stack means
		 we can no longer use reg + offset to represent CFA.
		 Here we use DW_CFA_def_cfa_expression instead.  The
		 result of this expression equals to the original CFA
		 value.  */
              if (fde
                  && fde->stack_realign
                  && cfa.indirect == 0
                  && cfa.reg != DW_FRAME_POINTER_REGNUM)
                {
		  dw_cfa_location cfa_exp;

		  gcc_assert (fde->drap_reg == cfa.reg);

		  cfa_exp.indirect = 1;
		  cfa_exp.reg = DW_FRAME_POINTER_REGNUM;
		  cfa_exp.base_offset = offset;
		  cfa_exp.offset = 0;

		  fde->drap_reg_saved = 1;

		  def_cfa_1 (&cfa_exp);
		  break;
                }

	      /* If the source register is exactly the CFA, assume
		 we're saving SP like any other register; this happens
		 on the ARM.  */
	      def_cfa_1 (&cfa);
	      queue_reg_save (stack_pointer_rtx, NULL_RTX, offset);
	      break;
	    }
	  else
	    {
	      /* Otherwise, we'll need to look in the stack to
		 calculate the CFA.  */
	      rtx x = XEXP (dest, 0);

	      if (!REG_P (x))
		x = XEXP (x, 0);
	      gcc_assert (REG_P (x));

	      cfa.reg = dwf_regno (x);
	      cfa.base_offset = offset;
	      cfa.indirect = 1;
	      def_cfa_1 (&cfa);
	      break;
	    }
	}

      def_cfa_1 (&cfa);

      span = NULL;
      if (REG_P (src))
	span = targetm.dwarf_register_span (src);
      if (!span)
	queue_reg_save (src, NULL_RTX, offset);
      else
	{
	  /* We have a PARALLEL describing where the contents of SRC live.
	     Queue register saves for each piece of the PARALLEL.  */
	  int par_index;
	  int limit;
	  HOST_WIDE_INT span_offset = offset;

	  gcc_assert (GET_CODE (span) == PARALLEL);

	  limit = XVECLEN (span, 0);
	  for (par_index = 0; par_index < limit; par_index++)
	    {
	      rtx elem = XVECEXP (span, 0, par_index);
	      queue_reg_save (elem, NULL_RTX, span_offset);
	      span_offset += GET_MODE_SIZE (GET_MODE (elem));
	    }
	}
      break;

    default:
      gcc_unreachable ();
    }
}

/* Record call frame debugging information for INSN, which either
   sets SP or FP (adjusting how we calculate the frame address) or saves a
   register to the stack.  If INSN is NULL_RTX, initialize our state.

   If AFTER_P is false, we're being called before the insn is emitted,
   otherwise after.  Call instructions get invoked twice.  */

static void
dwarf2out_frame_debug (rtx insn, bool after_p)
{
  rtx note, n;
  bool handled_one = false;
  bool need_flush = false;

  if (!NONJUMP_INSN_P (insn) || clobbers_queued_reg_save (insn))
    dwarf2out_flush_queued_reg_saves ();

  if (!RTX_FRAME_RELATED_P (insn))
    {
      /* ??? This should be done unconditionally since stack adjustments
	 matter if the stack pointer is not the CFA register anymore but
	 is still used to save registers.  */
      if (!ACCUMULATE_OUTGOING_ARGS)
	dwarf2out_notice_stack_adjust (insn, after_p);
      return;
    }

  any_cfis_emitted = false;

  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
    switch (REG_NOTE_KIND (note))
      {
      case REG_FRAME_RELATED_EXPR:
	insn = XEXP (note, 0);
	goto do_frame_expr;

      case REG_CFA_DEF_CFA:
	dwarf2out_frame_debug_def_cfa (XEXP (note, 0));
	handled_one = true;
	break;

      case REG_CFA_ADJUST_CFA:
	n = XEXP (note, 0);
	if (n == NULL)
	  {
	    n = PATTERN (insn);
	    if (GET_CODE (n) == PARALLEL)
	      n = XVECEXP (n, 0, 0);
	  }
	dwarf2out_frame_debug_adjust_cfa (n);
	handled_one = true;
	break;

      case REG_CFA_OFFSET:
	n = XEXP (note, 0);
	if (n == NULL)
	  n = single_set (insn);
	dwarf2out_frame_debug_cfa_offset (n);
	handled_one = true;
	break;

      case REG_CFA_REGISTER:
	n = XEXP (note, 0);
	if (n == NULL)
	  {
	    n = PATTERN (insn);
	    if (GET_CODE (n) == PARALLEL)
	      n = XVECEXP (n, 0, 0);
	  }
	dwarf2out_frame_debug_cfa_register (n);
	handled_one = true;
	break;

      case REG_CFA_EXPRESSION:
	n = XEXP (note, 0);
	if (n == NULL)
	  n = single_set (insn);
	dwarf2out_frame_debug_cfa_expression (n);
	handled_one = true;
	break;

      case REG_CFA_RESTORE:
	n = XEXP (note, 0);
	if (n == NULL)
	  {
	    n = PATTERN (insn);
	    if (GET_CODE (n) == PARALLEL)
	      n = XVECEXP (n, 0, 0);
	    n = XEXP (n, 0);
	  }
	dwarf2out_frame_debug_cfa_restore (n);
	handled_one = true;
	break;

      case REG_CFA_SET_VDRAP:
	n = XEXP (note, 0);
	if (REG_P (n))
	  {
	    dw_fde_ref fde = cfun->fde;
	    if (fde)
	      {
		gcc_assert (fde->vdrap_reg == INVALID_REGNUM);
		if (REG_P (n))
		  fde->vdrap_reg = dwf_regno (n);
	      }
	  }
	handled_one = true;
	break;

      case REG_CFA_WINDOW_SAVE:
	dwarf2out_frame_debug_cfa_window_save ();
	handled_one = true;
	break;

      case REG_CFA_FLUSH_QUEUE:
	/* The actual flush happens below.  */
	need_flush = true;
	handled_one = true;
	break;

      default:
	break;
      }

  if (handled_one)
    {
      /* Minimize the number of advances by emitting the entire queue
	 once anything is emitted.  */
      need_flush |= any_cfis_emitted;
    }
  else
    {
      insn = PATTERN (insn);
    do_frame_expr:
      dwarf2out_frame_debug_expr (insn);

      /* Check again.  A parallel can save and update the same register.
         We could probably check just once, here, but this is safer than
         removing the check at the start of the function.  */
      if (any_cfis_emitted || clobbers_queued_reg_save (insn))
	need_flush = true;
    }

  if (need_flush)
    dwarf2out_flush_queued_reg_saves ();
}

/* Examine CFI and return true if a cfi label and set_loc is needed
   beforehand.  Even when generating CFI assembler instructions, we
   still have to add the cfi to the list so that lookup_cfa_1 works
   later on.  When -g2 and above we even need to force emitting of
   CFI labels and add to list a DW_CFA_set_loc for convert_cfa_to_fb_loc_list
   purposes.  If we're generating DWARF3 output we use DW_OP_call_frame_cfa
   and so don't use convert_cfa_to_fb_loc_list.  */

static bool
cfi_label_required_p (dw_cfi_ref cfi)
{
  if (!dwarf2out_do_cfi_asm ())
    return true;

  if (dwarf_version == 2
      && debug_info_level > DINFO_LEVEL_TERSE
      && (write_symbols == DWARF2_DEBUG
	  || write_symbols == VMS_AND_DWARF2_DEBUG))
    {
      switch (cfi->dw_cfi_opc)
	{
	case DW_CFA_def_cfa_offset:
	case DW_CFA_def_cfa_offset_sf:
	case DW_CFA_def_cfa_register:
	case DW_CFA_def_cfa:
	case DW_CFA_def_cfa_sf:
	case DW_CFA_def_cfa_expression:
	case DW_CFA_restore_state:
	  return true;
	default:
	  return false;
	}
    }
  return false;
}

/* Walk the function, looking for NOTE_INSN_CFI notes.  Add the CFIs to the
   function's FDE, adding CFI labels and set_loc/advance_loc opcodes as
   necessary.  */
static void
add_cfis_to_fde (void)
{
  dw_fde_ref fde = cfun->fde;
  rtx insn, next;
  /* We always start with a function_begin label.  */
  bool first = false;

  for (insn = get_insns (); insn; insn = next)
    {
      next = NEXT_INSN (insn);

      if (NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_SWITCH_TEXT_SECTIONS)
	{
	  /* Don't attempt to advance_loc4 between labels
	     in different sections.  */
	  first = true;
	}

      if (NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_CFI)
	{
	  bool required = cfi_label_required_p (NOTE_CFI (insn));
	  while (next && NOTE_P (next) && NOTE_KIND (next) == NOTE_INSN_CFI)
	    {
	      required |= cfi_label_required_p (NOTE_CFI (next));
	      next = NEXT_INSN (next);
	    }
	  if (required)
	    {
	      int num = dwarf2out_cfi_label_num;
	      const char *label = dwarf2out_cfi_label ();
	      dw_cfi_ref xcfi;
	      rtx tmp;

	      /* Set the location counter to the new label.  */
	      xcfi = new_cfi ();
	      xcfi->dw_cfi_opc = (first ? DW_CFA_set_loc
				  : DW_CFA_advance_loc4);
	      xcfi->dw_cfi_oprnd1.dw_cfi_addr = label;
	      VEC_safe_push (dw_cfi_ref, gc, fde->dw_fde_cfi, xcfi);

	      tmp = emit_note_before (NOTE_INSN_CFI_LABEL, insn);
	      NOTE_LABEL_NUMBER (tmp) = num;
	    }

	  do
	    {
	      VEC_safe_push (dw_cfi_ref, gc, fde->dw_fde_cfi, NOTE_CFI (insn));
	      insn = NEXT_INSN (insn);
	    }
	  while (insn != next);
	  first = false;
	}
    }
}

/* Scan the function and create the initial set of CFI notes.  */
 
static void
create_cfi_notes (void)
{
  rtx insn;

  for (insn = get_insns (); insn ; insn = NEXT_INSN (insn))
    {
      rtx pat;

      cfi_insn = PREV_INSN (insn);

      if (BARRIER_P (insn))
	{
	  dwarf2out_frame_debug (insn, false);
	  continue;
        }

      if (NOTE_P (insn))
	{
	  switch (NOTE_KIND (insn))
	    {
	    case NOTE_INSN_PROLOGUE_END:
	      dwarf2out_flush_queued_reg_saves ();
	      break;

	    case NOTE_INSN_EPILOGUE_BEG:
#if defined(HAVE_epilogue)
	      dwarf2out_cfi_begin_epilogue (insn);
#endif
	      break;

	    case NOTE_INSN_CFA_RESTORE_STATE:
	      cfi_insn = insn;
	      dwarf2out_frame_debug_restore_state ();
	      break;
	    }
	  continue;
	}

      if (!NONDEBUG_INSN_P (insn))
	continue;

      pat = PATTERN (insn);
      if (asm_noperands (pat) >= 0)
	{
	  dwarf2out_frame_debug (insn, false);
	  continue;
	}

      if (GET_CODE (pat) == SEQUENCE)
	{
	  int i, n = XVECLEN (pat, 0);
	  for (i = 1; i < n; ++i)
	    dwarf2out_frame_debug (XVECEXP (pat, 0, i), false);
	}

      if (CALL_P (insn)
	  || find_reg_note (insn, REG_CFA_FLUSH_QUEUE, NULL))
	dwarf2out_frame_debug (insn, false);

      /* Do not separate tablejump insns from their ADDR_DIFF_VEC.
	 Putting the note after the VEC should be ok.  */
      if (!tablejump_p (insn, NULL, &cfi_insn))
	cfi_insn = insn;

      dwarf2out_frame_debug (insn, true);
    }

  cfi_insn = NULL;
}

/* Determine if we need to save and restore CFI information around this
   epilogue.  If SIBCALL is true, then this is a sibcall epilogue.  If
   we do need to save/restore, then emit the save now, and insert a
   NOTE_INSN_CFA_RESTORE_STATE at the appropriate place in the stream.  */

static void
dwarf2out_cfi_begin_epilogue (rtx insn)
{
  bool saw_frp = false;
  rtx i;

  /* Scan forward to the return insn, noticing if there are possible
     frame related insns.  */
  for (i = NEXT_INSN (insn); i ; i = NEXT_INSN (i))
    {
      if (!INSN_P (i))
	continue;

      /* Look for both regular and sibcalls to end the block.  */
      if (returnjump_p (i))
	break;
      if (CALL_P (i) && SIBLING_CALL_P (i))
	break;

      if (GET_CODE (PATTERN (i)) == SEQUENCE)
	{
	  int idx;
	  rtx seq = PATTERN (i);

	  if (returnjump_p (XVECEXP (seq, 0, 0)))
	    break;
	  if (CALL_P (XVECEXP (seq, 0, 0))
	      && SIBLING_CALL_P (XVECEXP (seq, 0, 0)))
	    break;

	  for (idx = 0; idx < XVECLEN (seq, 0); idx++)
	    if (RTX_FRAME_RELATED_P (XVECEXP (seq, 0, idx)))
	      saw_frp = true;
	}

      if (RTX_FRAME_RELATED_P (i))
	saw_frp = true;
    }

  /* If the port doesn't emit epilogue unwind info, we don't need a
     save/restore pair.  */
  if (!saw_frp)
    return;

  /* Otherwise, search forward to see if the return insn was the last
     basic block of the function.  If so, we don't need save/restore.  */
  gcc_assert (i != NULL);
  i = next_real_insn (i);
  if (i == NULL)
    return;

  /* Insert the restore before that next real insn in the stream, and before
     a potential NOTE_INSN_EPILOGUE_BEG -- we do need these notes to be
     properly nested.  This should be after any label or alignment.  This
     will be pushed into the CFI stream by the function below.  */
  while (1)
    {
      rtx p = PREV_INSN (i);
      if (!NOTE_P (p))
	break;
      if (NOTE_KIND (p) == NOTE_INSN_BASIC_BLOCK)
	break;
      i = p;
    }
  emit_note_before (NOTE_INSN_CFA_RESTORE_STATE, i);

  emit_cfa_remember = true;

  /* And emulate the state save.  */
  gcc_assert (!cfa_remember.in_use);
  cfa_remember = cfa;
  old_cfa_remember = old_cfa;
  cfa_remember.in_use = 1;
}

/* A "subroutine" of dwarf2out_cfi_begin_epilogue.  Emit the restore
   required.  */

static void
dwarf2out_frame_debug_restore_state (void)
{
  dw_cfi_ref cfi = new_cfi ();

  cfi->dw_cfi_opc = DW_CFA_restore_state;
  add_cfi (cfi);

  gcc_assert (cfa_remember.in_use);
  cfa = cfa_remember;
  old_cfa = old_cfa_remember;
  cfa_remember.in_use = 0;
}

/* Record the initial position of the return address.  RTL is
   INCOMING_RETURN_ADDR_RTX.  */

static void
initial_return_save (rtx rtl)
{
  unsigned int reg = INVALID_REGNUM;
  HOST_WIDE_INT offset = 0;

  switch (GET_CODE (rtl))
    {
    case REG:
      /* RA is in a register.  */
      reg = dwf_regno (rtl);
      break;

    case MEM:
      /* RA is on the stack.  */
      rtl = XEXP (rtl, 0);
      switch (GET_CODE (rtl))
	{
	case REG:
	  gcc_assert (REGNO (rtl) == STACK_POINTER_REGNUM);
	  offset = 0;
	  break;

	case PLUS:
	  gcc_assert (REGNO (XEXP (rtl, 0)) == STACK_POINTER_REGNUM);
	  offset = INTVAL (XEXP (rtl, 1));
	  break;

	case MINUS:
	  gcc_assert (REGNO (XEXP (rtl, 0)) == STACK_POINTER_REGNUM);
	  offset = -INTVAL (XEXP (rtl, 1));
	  break;

	default:
	  gcc_unreachable ();
	}

      break;

    case PLUS:
      /* The return address is at some offset from any value we can
	 actually load.  For instance, on the SPARC it is in %i7+8. Just
	 ignore the offset for now; it doesn't matter for unwinding frames.  */
      gcc_assert (CONST_INT_P (XEXP (rtl, 1)));
      initial_return_save (XEXP (rtl, 0));
      return;

    default:
      gcc_unreachable ();
    }

  if (reg != DWARF_FRAME_RETURN_COLUMN)
    {
      if (reg != INVALID_REGNUM)
        record_reg_saved_in_reg (rtl, pc_rtx);
      reg_save (DWARF_FRAME_RETURN_COLUMN, reg, offset - cfa.offset);
    }
}

/* Annotate the function with NOTE_INSN_CFI notes to record the CFI
   state at each location within the function.  These notes will be
   emitted during pass_final.  */

static unsigned int
execute_dwarf2_frame (void)
{
  /* The first time we're called, compute the incoming frame state.  */
  if (cie_cfi_vec == NULL)
    {
      dw_cfa_location loc;

      add_cfi_vec = &cie_cfi_vec;

      memset(&old_cfa, 0, sizeof (old_cfa));
      old_cfa.reg = INVALID_REGNUM;

      /* On entry, the Canonical Frame Address is at SP.  */
      memset(&loc, 0, sizeof (loc));
      loc.reg = DW_STACK_POINTER_REGNUM;
      loc.offset = INCOMING_FRAME_SP_OFFSET;
      def_cfa_1 (&loc);

      if (targetm.debug_unwind_info () == UI_DWARF2
          || targetm_common.except_unwind_info (&global_options) == UI_DWARF2)
	{
	  initial_return_save (INCOMING_RETURN_ADDR_RTX);

	  /* For a few targets, we have the return address incoming into a
	     register, but choose a different return column.  This will result
	     in a DW_CFA_register for the return, and an entry in
	     regs_saved_in_regs to match.  If the target later stores that
	     return address register to the stack, we want to be able to emit
	     the DW_CFA_offset against the return column, not the intermediate
	     save register.  Save the contents of regs_saved_in_regs so that
	     we can re-initialize it at the start of each function.  */
	  switch (VEC_length (reg_saved_in_data, regs_saved_in_regs))
	    {
	    case 0:
	      break;
	    case 1:
	      cie_return_save = ggc_alloc_reg_saved_in_data ();
	      *cie_return_save = *VEC_index (reg_saved_in_data,
					     regs_saved_in_regs, 0);
	      regs_saved_in_regs = NULL;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	}

      add_cfi_vec = NULL;
    }

  /* Set up state for generating call frame debug info.  */
  gcc_checking_assert (queued_reg_saves == NULL);
  gcc_checking_assert (regs_saved_in_regs == NULL);

  memset (&cfa, 0, sizeof(cfa));
  cfa.reg = DW_STACK_POINTER_REGNUM;
  cfa.offset = INCOMING_FRAME_SP_OFFSET;

  old_cfa = cfa;
  cfa_store = cfa;

  memset (&cfa_temp, 0, sizeof(cfa_temp));
  cfa_temp.reg = INVALID_REGNUM;

  if (cie_return_save)
    VEC_safe_push (reg_saved_in_data, gc, regs_saved_in_regs, cie_return_save);

  dwarf2out_alloc_current_fde ();

  /* Do the work.  */
  create_cfi_notes ();
  add_cfis_to_fde ();

  /* Reset all function-specific information, particularly for GC.  */
  XDELETEVEC (barrier_args_size);
  barrier_args_size = NULL;
  regs_saved_in_regs = NULL;
  queued_reg_saves = NULL;

  return 0;
}

/* Convert a DWARF call frame info. operation to its string name */

static const char *
dwarf_cfi_name (unsigned int cfi_opc)
{
  switch (cfi_opc)
    {
    case DW_CFA_advance_loc:
      return "DW_CFA_advance_loc";
    case DW_CFA_offset:
      return "DW_CFA_offset";
    case DW_CFA_restore:
      return "DW_CFA_restore";
    case DW_CFA_nop:
      return "DW_CFA_nop";
    case DW_CFA_set_loc:
      return "DW_CFA_set_loc";
    case DW_CFA_advance_loc1:
      return "DW_CFA_advance_loc1";
    case DW_CFA_advance_loc2:
      return "DW_CFA_advance_loc2";
    case DW_CFA_advance_loc4:
      return "DW_CFA_advance_loc4";
    case DW_CFA_offset_extended:
      return "DW_CFA_offset_extended";
    case DW_CFA_restore_extended:
      return "DW_CFA_restore_extended";
    case DW_CFA_undefined:
      return "DW_CFA_undefined";
    case DW_CFA_same_value:
      return "DW_CFA_same_value";
    case DW_CFA_register:
      return "DW_CFA_register";
    case DW_CFA_remember_state:
      return "DW_CFA_remember_state";
    case DW_CFA_restore_state:
      return "DW_CFA_restore_state";
    case DW_CFA_def_cfa:
      return "DW_CFA_def_cfa";
    case DW_CFA_def_cfa_register:
      return "DW_CFA_def_cfa_register";
    case DW_CFA_def_cfa_offset:
      return "DW_CFA_def_cfa_offset";

    /* DWARF 3 */
    case DW_CFA_def_cfa_expression:
      return "DW_CFA_def_cfa_expression";
    case DW_CFA_expression:
      return "DW_CFA_expression";
    case DW_CFA_offset_extended_sf:
      return "DW_CFA_offset_extended_sf";
    case DW_CFA_def_cfa_sf:
      return "DW_CFA_def_cfa_sf";
    case DW_CFA_def_cfa_offset_sf:
      return "DW_CFA_def_cfa_offset_sf";

    /* SGI/MIPS specific */
    case DW_CFA_MIPS_advance_loc8:
      return "DW_CFA_MIPS_advance_loc8";

    /* GNU extensions */
    case DW_CFA_GNU_window_save:
      return "DW_CFA_GNU_window_save";
    case DW_CFA_GNU_args_size:
      return "DW_CFA_GNU_args_size";
    case DW_CFA_GNU_negative_offset_extended:
      return "DW_CFA_GNU_negative_offset_extended";

    default:
      return "DW_CFA_<unknown>";
    }
}

/* This routine will generate the correct assembly data for a location
   description based on a cfi entry with a complex address.  */

static void
output_cfa_loc (dw_cfi_ref cfi, int for_eh)
{
  dw_loc_descr_ref loc;
  unsigned long size;

  if (cfi->dw_cfi_opc == DW_CFA_expression)
    {
      unsigned r = 
	DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, for_eh);
      dw2_asm_output_data (1, r, NULL);
      loc = cfi->dw_cfi_oprnd2.dw_cfi_loc;
    }
  else
    loc = cfi->dw_cfi_oprnd1.dw_cfi_loc;

  /* Output the size of the block.  */
  size = size_of_locs (loc);
  dw2_asm_output_data_uleb128 (size, NULL);

  /* Now output the operations themselves.  */
  output_loc_sequence (loc, for_eh);
}

/* Similar, but used for .cfi_escape.  */

static void
output_cfa_loc_raw (dw_cfi_ref cfi)
{
  dw_loc_descr_ref loc;
  unsigned long size;

  if (cfi->dw_cfi_opc == DW_CFA_expression)
    {
      unsigned r = 
	DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, 1);
      fprintf (asm_out_file, "%#x,", r);
      loc = cfi->dw_cfi_oprnd2.dw_cfi_loc;
    }
  else
    loc = cfi->dw_cfi_oprnd1.dw_cfi_loc;

  /* Output the size of the block.  */
  size = size_of_locs (loc);
  dw2_asm_output_data_uleb128_raw (size);
  fputc (',', asm_out_file);

  /* Now output the operations themselves.  */
  output_loc_sequence_raw (loc);
}

/* Output a Call Frame Information opcode and its operand(s).  */

void
output_cfi (dw_cfi_ref cfi, dw_fde_ref fde, int for_eh)
{
  unsigned long r;
  HOST_WIDE_INT off;

  if (cfi->dw_cfi_opc == DW_CFA_advance_loc)
    dw2_asm_output_data (1, (cfi->dw_cfi_opc
			     | (cfi->dw_cfi_oprnd1.dw_cfi_offset & 0x3f)),
			 "DW_CFA_advance_loc " HOST_WIDE_INT_PRINT_HEX,
			 ((unsigned HOST_WIDE_INT)
			  cfi->dw_cfi_oprnd1.dw_cfi_offset));
  else if (cfi->dw_cfi_opc == DW_CFA_offset)
    {
      r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, for_eh);
      dw2_asm_output_data (1, (cfi->dw_cfi_opc | (r & 0x3f)),
			   "DW_CFA_offset, column %#lx", r);
      off = div_data_align (cfi->dw_cfi_oprnd2.dw_cfi_offset);
      dw2_asm_output_data_uleb128 (off, NULL);
    }
  else if (cfi->dw_cfi_opc == DW_CFA_restore)
    {
      r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, for_eh);
      dw2_asm_output_data (1, (cfi->dw_cfi_opc | (r & 0x3f)),
			   "DW_CFA_restore, column %#lx", r);
    }
  else
    {
      dw2_asm_output_data (1, cfi->dw_cfi_opc,
			   "%s", dwarf_cfi_name (cfi->dw_cfi_opc));

      switch (cfi->dw_cfi_opc)
	{
	case DW_CFA_set_loc:
	  if (for_eh)
	    dw2_asm_output_encoded_addr_rtx (
		ASM_PREFERRED_EH_DATA_FORMAT (/*code=*/1, /*global=*/0),
		gen_rtx_SYMBOL_REF (Pmode, cfi->dw_cfi_oprnd1.dw_cfi_addr),
		false, NULL);
	  else
	    dw2_asm_output_addr (DWARF2_ADDR_SIZE,
				 cfi->dw_cfi_oprnd1.dw_cfi_addr, NULL);
	  fde->dw_fde_current_label = cfi->dw_cfi_oprnd1.dw_cfi_addr;
	  break;

	case DW_CFA_advance_loc1:
	  dw2_asm_output_delta (1, cfi->dw_cfi_oprnd1.dw_cfi_addr,
				fde->dw_fde_current_label, NULL);
	  fde->dw_fde_current_label = cfi->dw_cfi_oprnd1.dw_cfi_addr;
	  break;

	case DW_CFA_advance_loc2:
	  dw2_asm_output_delta (2, cfi->dw_cfi_oprnd1.dw_cfi_addr,
				fde->dw_fde_current_label, NULL);
	  fde->dw_fde_current_label = cfi->dw_cfi_oprnd1.dw_cfi_addr;
	  break;

	case DW_CFA_advance_loc4:
	  dw2_asm_output_delta (4, cfi->dw_cfi_oprnd1.dw_cfi_addr,
				fde->dw_fde_current_label, NULL);
	  fde->dw_fde_current_label = cfi->dw_cfi_oprnd1.dw_cfi_addr;
	  break;

	case DW_CFA_MIPS_advance_loc8:
	  dw2_asm_output_delta (8, cfi->dw_cfi_oprnd1.dw_cfi_addr,
				fde->dw_fde_current_label, NULL);
	  fde->dw_fde_current_label = cfi->dw_cfi_oprnd1.dw_cfi_addr;
	  break;

	case DW_CFA_offset_extended:
	  r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, for_eh);
	  dw2_asm_output_data_uleb128 (r, NULL);
	  off = div_data_align (cfi->dw_cfi_oprnd2.dw_cfi_offset);
	  dw2_asm_output_data_uleb128 (off, NULL);
	  break;

	case DW_CFA_def_cfa:
	  r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, for_eh);
	  dw2_asm_output_data_uleb128 (r, NULL);
	  dw2_asm_output_data_uleb128 (cfi->dw_cfi_oprnd2.dw_cfi_offset, NULL);
	  break;

	case DW_CFA_offset_extended_sf:
	  r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, for_eh);
	  dw2_asm_output_data_uleb128 (r, NULL);
	  off = div_data_align (cfi->dw_cfi_oprnd2.dw_cfi_offset);
	  dw2_asm_output_data_sleb128 (off, NULL);
	  break;

	case DW_CFA_def_cfa_sf:
	  r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, for_eh);
	  dw2_asm_output_data_uleb128 (r, NULL);
	  off = div_data_align (cfi->dw_cfi_oprnd2.dw_cfi_offset);
	  dw2_asm_output_data_sleb128 (off, NULL);
	  break;

	case DW_CFA_restore_extended:
	case DW_CFA_undefined:
	case DW_CFA_same_value:
	case DW_CFA_def_cfa_register:
	  r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, for_eh);
	  dw2_asm_output_data_uleb128 (r, NULL);
	  break;

	case DW_CFA_register:
	  r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, for_eh);
	  dw2_asm_output_data_uleb128 (r, NULL);
	  r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd2.dw_cfi_reg_num, for_eh);
	  dw2_asm_output_data_uleb128 (r, NULL);
	  break;

	case DW_CFA_def_cfa_offset:
	case DW_CFA_GNU_args_size:
	  dw2_asm_output_data_uleb128 (cfi->dw_cfi_oprnd1.dw_cfi_offset, NULL);
	  break;

	case DW_CFA_def_cfa_offset_sf:
	  off = div_data_align (cfi->dw_cfi_oprnd1.dw_cfi_offset);
	  dw2_asm_output_data_sleb128 (off, NULL);
	  break;

	case DW_CFA_GNU_window_save:
	  break;

	case DW_CFA_def_cfa_expression:
	case DW_CFA_expression:
	  output_cfa_loc (cfi, for_eh);
	  break;

	case DW_CFA_GNU_negative_offset_extended:
	  /* Obsoleted by DW_CFA_offset_extended_sf.  */
	  gcc_unreachable ();

	default:
	  break;
	}
    }
}

/* Similar, but do it via assembler directives instead.  */

void
output_cfi_directive (FILE *f, dw_cfi_ref cfi)
{
  unsigned long r, r2;

  switch (cfi->dw_cfi_opc)
    {
    case DW_CFA_advance_loc:
    case DW_CFA_advance_loc1:
    case DW_CFA_advance_loc2:
    case DW_CFA_advance_loc4:
    case DW_CFA_MIPS_advance_loc8:
    case DW_CFA_set_loc:
      /* Should only be created in a code path not followed when emitting
	 via directives.  The assembler is going to take care of this for
	 us.  But this routines is also used for debugging dumps, so
	 print something.  */
      gcc_assert (f != asm_out_file);
      fprintf (f, "\t.cfi_advance_loc\n");
      break;

    case DW_CFA_offset:
    case DW_CFA_offset_extended:
    case DW_CFA_offset_extended_sf:
      r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, 1);
      fprintf (f, "\t.cfi_offset %lu, "HOST_WIDE_INT_PRINT_DEC"\n",
	       r, cfi->dw_cfi_oprnd2.dw_cfi_offset);
      break;

    case DW_CFA_restore:
    case DW_CFA_restore_extended:
      r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, 1);
      fprintf (f, "\t.cfi_restore %lu\n", r);
      break;

    case DW_CFA_undefined:
      r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, 1);
      fprintf (f, "\t.cfi_undefined %lu\n", r);
      break;

    case DW_CFA_same_value:
      r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, 1);
      fprintf (f, "\t.cfi_same_value %lu\n", r);
      break;

    case DW_CFA_def_cfa:
    case DW_CFA_def_cfa_sf:
      r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, 1);
      fprintf (f, "\t.cfi_def_cfa %lu, "HOST_WIDE_INT_PRINT_DEC"\n",
	       r, cfi->dw_cfi_oprnd2.dw_cfi_offset);
      break;

    case DW_CFA_def_cfa_register:
      r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, 1);
      fprintf (f, "\t.cfi_def_cfa_register %lu\n", r);
      break;

    case DW_CFA_register:
      r = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd1.dw_cfi_reg_num, 1);
      r2 = DWARF2_FRAME_REG_OUT (cfi->dw_cfi_oprnd2.dw_cfi_reg_num, 1);
      fprintf (f, "\t.cfi_register %lu, %lu\n", r, r2);
      break;

    case DW_CFA_def_cfa_offset:
    case DW_CFA_def_cfa_offset_sf:
      fprintf (f, "\t.cfi_def_cfa_offset "
	       HOST_WIDE_INT_PRINT_DEC"\n",
	       cfi->dw_cfi_oprnd1.dw_cfi_offset);
      break;

    case DW_CFA_remember_state:
      fprintf (f, "\t.cfi_remember_state\n");
      break;
    case DW_CFA_restore_state:
      fprintf (f, "\t.cfi_restore_state\n");
      break;

    case DW_CFA_GNU_args_size:
      if (f == asm_out_file)
	{
	  fprintf (f, "\t.cfi_escape %#x,", DW_CFA_GNU_args_size);
	  dw2_asm_output_data_uleb128_raw (cfi->dw_cfi_oprnd1.dw_cfi_offset);
	  if (flag_debug_asm)
	    fprintf (f, "\t%s args_size "HOST_WIDE_INT_PRINT_DEC,
		     ASM_COMMENT_START, cfi->dw_cfi_oprnd1.dw_cfi_offset);
	  fputc ('\n', f);
	}
      else
	{
	  fprintf (f, "\t.cfi_GNU_args_size "HOST_WIDE_INT_PRINT_DEC "\n",
		   cfi->dw_cfi_oprnd1.dw_cfi_offset);
	}
      break;

    case DW_CFA_GNU_window_save:
      fprintf (f, "\t.cfi_window_save\n");
      break;

    case DW_CFA_def_cfa_expression:
      if (f != asm_out_file)
	{
	  fprintf (f, "\t.cfi_def_cfa_expression ...\n");
	  break;
	}
      /* FALLTHRU */
    case DW_CFA_expression:
      if (f != asm_out_file)
	{
	  fprintf (f, "\t.cfi_cfa_expression ...\n");
	  break;
	}
      fprintf (f, "\t.cfi_escape %#x,", cfi->dw_cfi_opc);
      output_cfa_loc_raw (cfi);
      fputc ('\n', f);
      break;

    default:
      gcc_unreachable ();
    }
}

void
dwarf2out_emit_cfi (dw_cfi_ref cfi)
{
  if (dwarf2out_do_cfi_asm ())
    output_cfi_directive (asm_out_file, cfi);
}

/* Output CFIs from VEC, up to index UPTO, to bring current FDE to the
   same state as after executing CFIs in CFI chain.  DO_CFI_ASM is
   true if .cfi_* directives shall be emitted, false otherwise.  If it
   is false, FDE and FOR_EH are the other arguments to pass to
   output_cfi.  */

void
output_cfis (cfi_vec vec, int upto, bool do_cfi_asm,
	     dw_fde_ref fde, bool for_eh)
{
  int ix;
  struct dw_cfi_struct cfi_buf;
  dw_cfi_ref cfi2;
  dw_cfi_ref cfi_args_size = NULL, cfi_cfa = NULL, cfi_cfa_offset = NULL;
  VEC(dw_cfi_ref, heap) *regs = VEC_alloc (dw_cfi_ref, heap, 32);
  unsigned int len, idx;

  for (ix = 0; ix < upto + 1; ix++)
    {
      dw_cfi_ref cfi = ix < upto ? VEC_index (dw_cfi_ref, vec, ix) : NULL;
      switch (cfi ? cfi->dw_cfi_opc : DW_CFA_nop)
	{
	case DW_CFA_advance_loc:
	case DW_CFA_advance_loc1:
	case DW_CFA_advance_loc2:
	case DW_CFA_advance_loc4:
	case DW_CFA_MIPS_advance_loc8:
	case DW_CFA_set_loc:
	  /* All advances should be ignored.  */
	  break;
	case DW_CFA_remember_state:
	  {
	    dw_cfi_ref args_size = cfi_args_size;

	    /* Skip everything between .cfi_remember_state and
	       .cfi_restore_state.  */
	    ix++;
	    if (ix == upto)
	      goto flush_all;

	    for (; ix < upto; ix++)
	      {
		cfi2 = VEC_index (dw_cfi_ref, vec, ix);
		if (cfi2->dw_cfi_opc == DW_CFA_restore_state)
		  break;
		else if (cfi2->dw_cfi_opc == DW_CFA_GNU_args_size)
		  args_size = cfi2;
		else
		  gcc_assert (cfi2->dw_cfi_opc != DW_CFA_remember_state);
	      }

	    cfi_args_size = args_size;
	    break;
	  }
	case DW_CFA_GNU_args_size:
	  cfi_args_size = cfi;
	  break;
	case DW_CFA_GNU_window_save:
	  goto flush_all;
	case DW_CFA_offset:
	case DW_CFA_offset_extended:
	case DW_CFA_offset_extended_sf:
	case DW_CFA_restore:
	case DW_CFA_restore_extended:
	case DW_CFA_undefined:
	case DW_CFA_same_value:
	case DW_CFA_register:
	case DW_CFA_val_offset:
	case DW_CFA_val_offset_sf:
	case DW_CFA_expression:
	case DW_CFA_val_expression:
	case DW_CFA_GNU_negative_offset_extended:
	  if (VEC_length (dw_cfi_ref, regs)
	      <= cfi->dw_cfi_oprnd1.dw_cfi_reg_num)
	    VEC_safe_grow_cleared (dw_cfi_ref, heap, regs,
				   cfi->dw_cfi_oprnd1.dw_cfi_reg_num + 1);
	  VEC_replace (dw_cfi_ref, regs, cfi->dw_cfi_oprnd1.dw_cfi_reg_num,
		       cfi);
	  break;
	case DW_CFA_def_cfa:
	case DW_CFA_def_cfa_sf:
	case DW_CFA_def_cfa_expression:
	  cfi_cfa = cfi;
	  cfi_cfa_offset = cfi;
	  break;
	case DW_CFA_def_cfa_register:
	  cfi_cfa = cfi;
	  break;
	case DW_CFA_def_cfa_offset:
	case DW_CFA_def_cfa_offset_sf:
	  cfi_cfa_offset = cfi;
	  break;
	case DW_CFA_nop:
	  gcc_assert (cfi == NULL);
	flush_all:
	  len = VEC_length (dw_cfi_ref, regs);
	  for (idx = 0; idx < len; idx++)
	    {
	      cfi2 = VEC_replace (dw_cfi_ref, regs, idx, NULL);
	      if (cfi2 != NULL
		  && cfi2->dw_cfi_opc != DW_CFA_restore
		  && cfi2->dw_cfi_opc != DW_CFA_restore_extended)
		{
		  if (do_cfi_asm)
		    output_cfi_directive (asm_out_file, cfi2);
		  else
		    output_cfi (cfi2, fde, for_eh);
		}
	    }
	  if (cfi_cfa && cfi_cfa_offset && cfi_cfa_offset != cfi_cfa)
	    {
	      gcc_assert (cfi_cfa->dw_cfi_opc != DW_CFA_def_cfa_expression);
	      cfi_buf = *cfi_cfa;
	      switch (cfi_cfa_offset->dw_cfi_opc)
		{
		case DW_CFA_def_cfa_offset:
		  cfi_buf.dw_cfi_opc = DW_CFA_def_cfa;
		  cfi_buf.dw_cfi_oprnd2 = cfi_cfa_offset->dw_cfi_oprnd1;
		  break;
		case DW_CFA_def_cfa_offset_sf:
		  cfi_buf.dw_cfi_opc = DW_CFA_def_cfa_sf;
		  cfi_buf.dw_cfi_oprnd2 = cfi_cfa_offset->dw_cfi_oprnd1;
		  break;
		case DW_CFA_def_cfa:
		case DW_CFA_def_cfa_sf:
		  cfi_buf.dw_cfi_opc = cfi_cfa_offset->dw_cfi_opc;
		  cfi_buf.dw_cfi_oprnd2 = cfi_cfa_offset->dw_cfi_oprnd2;
		  break;
		default:
		  gcc_unreachable ();
		}
	      cfi_cfa = &cfi_buf;
	    }
	  else if (cfi_cfa_offset)
	    cfi_cfa = cfi_cfa_offset;
	  if (cfi_cfa)
	    {
	      if (do_cfi_asm)
		output_cfi_directive (asm_out_file, cfi_cfa);
	      else
		output_cfi (cfi_cfa, fde, for_eh);
	    }
	  cfi_cfa = NULL;
	  cfi_cfa_offset = NULL;
	  if (cfi_args_size
	      && cfi_args_size->dw_cfi_oprnd1.dw_cfi_offset)
	    {
	      if (do_cfi_asm)
		output_cfi_directive (asm_out_file, cfi_args_size);
	      else
		output_cfi (cfi_args_size, fde, for_eh);
	    }
	  cfi_args_size = NULL;
	  if (cfi == NULL)
	    {
	      VEC_free (dw_cfi_ref, heap, regs);
	      return;
	    }
	  else if (do_cfi_asm)
	    output_cfi_directive (asm_out_file, cfi);
	  else
	    output_cfi (cfi, fde, for_eh);
	  break;
	default:
	  gcc_unreachable ();
	}
    }
}


/* Save the result of dwarf2out_do_frame across PCH.
   This variable is tri-state, with 0 unset, >0 true, <0 false.  */
static GTY(()) signed char saved_do_cfi_asm = 0;

/* Decide whether we want to emit frame unwind information for the current
   translation unit.  */

bool
dwarf2out_do_frame (void)
{
  /* We want to emit correct CFA location expressions or lists, so we
     have to return true if we're going to output debug info, even if
     we're not going to output frame or unwind info.  */
  if (write_symbols == DWARF2_DEBUG || write_symbols == VMS_AND_DWARF2_DEBUG)
    return true;

  if (saved_do_cfi_asm > 0)
    return true;

  if (targetm.debug_unwind_info () == UI_DWARF2)
    return true;

  if ((flag_unwind_tables || flag_exceptions)
      && targetm_common.except_unwind_info (&global_options) == UI_DWARF2)
    return true;

  return false;
}

/* Decide whether to emit frame unwind via assembler directives.  */

bool
dwarf2out_do_cfi_asm (void)
{
  int enc;

#ifdef MIPS_DEBUGGING_INFO
  return false;
#endif

  if (saved_do_cfi_asm != 0)
    return saved_do_cfi_asm > 0;

  /* Assume failure for a moment.  */
  saved_do_cfi_asm = -1;

  if (!flag_dwarf2_cfi_asm || !dwarf2out_do_frame ())
    return false;
  if (!HAVE_GAS_CFI_PERSONALITY_DIRECTIVE)
    return false;

  /* Make sure the personality encoding is one the assembler can support.
     In particular, aligned addresses can't be handled.  */
  enc = ASM_PREFERRED_EH_DATA_FORMAT (/*code=*/2,/*global=*/1);
  if ((enc & 0x70) != 0 && (enc & 0x70) != DW_EH_PE_pcrel)
    return false;
  enc = ASM_PREFERRED_EH_DATA_FORMAT (/*code=*/0,/*global=*/0);
  if ((enc & 0x70) != 0 && (enc & 0x70) != DW_EH_PE_pcrel)
    return false;

  /* If we can't get the assembler to emit only .debug_frame, and we don't need
     dwarf2 unwind info for exceptions, then emit .debug_frame by hand.  */
  if (!HAVE_GAS_CFI_SECTIONS_DIRECTIVE
      && !flag_unwind_tables && !flag_exceptions
      && targetm_common.except_unwind_info (&global_options) != UI_DWARF2)
    return false;

  /* Success!  */
  saved_do_cfi_asm = 1;
  return true;
}

static bool
gate_dwarf2_frame (void)
{
#ifndef HAVE_prologue
  /* Targets which still implement the prologue in assembler text
     cannot use the generic dwarf2 unwinding.  */
  return false;
#endif

  /* ??? What to do for UI_TARGET unwinding?  They might be able to benefit
     from the optimized shrink-wrapping annotations that we will compute.
     For now, only produce the CFI notes for dwarf2.  */
  return dwarf2out_do_frame ();
}

struct rtl_opt_pass pass_dwarf2_frame =
{
 {
  RTL_PASS,
  "dwarf2",			/* name */
  gate_dwarf2_frame,		/* gate */
  execute_dwarf2_frame,		/* execute */
  NULL,				/* sub */
  NULL,				/* next */
  0,				/* static_pass_number */
  TV_FINAL,			/* tv_id */
  0,				/* properties_required */
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  0				/* todo_flags_finish */
 }
};

#include "gt-dwarf2cfi.h"
