/* Subroutines used for code generation on Xilinx MicroBlaze.
   Copyright 2009, 2010, 2011 Free Software Foundation, Inc.

   Contributed by Michael Eager <eager@eagercon.com>.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "insn-attr.h"
#include "recog.h"
#include "tree.h"
#include "function.h"
#include "expr.h"
#include "flags.h"
#include "reload.h"
#include "output.h"
#include "ggc.h"
#include "hashtab.h"
#include "target.h"
#include "target-def.h"
#include "tm_p.h"
#include "gstab.h"
#include "df.h"
#include "optabs.h"
#include "diagnostic-core.h"
#include "cgraph.h"

#define MICROBLAZE_VERSION_COMPARE(VA,VB) strcasecmp (VA, VB)

/* Classifies an address.

ADDRESS_INVALID
An invalid address.

ADDRESS_REG

A natural register or a register + const_int offset address.  
The register satisfies microblaze_valid_base_register_p and the 
offset is a const_arith_operand.

ADDRESS_REG_INDEX

A natural register offset by the index contained in an index register. The base
register satisfies microblaze_valid_base_register_p and the index register
satisfies microblaze_valid_index_register_p

ADDRESS_CONST_INT

A signed 16/32-bit constant address.

ADDRESS_SYMBOLIC:

A constant symbolic address or a (register + symbol).  */

enum microblaze_address_type
{
  ADDRESS_INVALID,
  ADDRESS_REG,
  ADDRESS_REG_INDEX,
  ADDRESS_CONST_INT,
  ADDRESS_SYMBOLIC,
  ADDRESS_GOTOFF,
  ADDRESS_PLT
};

/* Classifies symbols

SYMBOL_TYPE_GENERAL
        
A general symbol.  */
enum microblaze_symbol_type
{
  SYMBOL_TYPE_INVALID,
  SYMBOL_TYPE_GENERAL
};

/* Classification of a MicroBlaze address.  */
struct microblaze_address_info
{
  enum microblaze_address_type type;
  rtx regA; 	/* Contains valid values on ADDRESS_REG, ADDRESS_REG_INDEX, 
     		   ADDRESS_SYMBOLIC.  */
  rtx regB; 	/* Contains valid values on ADDRESS_REG_INDEX.  */
  rtx offset; 	/* Contains valid values on ADDRESS_CONST_INT and ADDRESS_REG.  */
  rtx symbol; 	/* Contains valid values on ADDRESS_SYMBOLIC.  */
  enum microblaze_symbol_type symbol_type;
};

/* Structure to be filled in by compute_frame_size with register
   save masks, and offsets for the current function.  */

struct GTY(()) microblaze_frame_info {
  long total_size;		/* # bytes that the entire frame takes up.  */
  long var_size;		/* # bytes that variables take up.  */
  long args_size;		/* # bytes that outgoing arguments take up.  */
  int link_debug_size;		/* # bytes for the link reg and back pointer.  */
  int gp_reg_size;		/* # bytes needed to store gp regs.  */
  long gp_offset;		/* offset from new sp to store gp registers.  */
  long mask;			/* mask of saved gp registers.  */
  int initialized;		/* != 0 if frame size already calculated.  */
  int num_gp;			/* number of gp registers saved.  */
  long insns_len;		/* length of insns.  */
  int alloc_stack;		/* Flag to indicate if the current function 
				   must not create stack space. (As an optimization).  */
};

/* Global variables for machine-dependent things.  */

/* Toggle which pipleline interface to use.  */
static GTY(()) int microblaze_sched_use_dfa = 0;

/* Threshold for data being put into the small data/bss area, instead
   of the normal data area (references to the small data/bss area take
   1 instruction, and use the global pointer, references to the normal
   data area takes 2 instructions).  */
int microblaze_section_threshold = -1;

/* Prevent scheduling potentially exception causing instructions in 
   delay slots.  -mcpu=v3.00.a or v4.00.a turns this on.  */
int microblaze_no_unsafe_delay;

/* Which CPU pipeline do we use. We haven't really standardized on a CPU 
   version having only a particular type of pipeline. There can still be 
   options on the CPU to scale pipeline features up or down. :( 
   Bad Presentation (??), so we let the MD file rely on the value of 
   this variable instead Making PIPE_5 the default. It should be backward 
   optimal with PIPE_3 MicroBlazes.  */
enum pipeline_type microblaze_pipe = MICROBLAZE_PIPE_5;

/* High and low marks for floating point values which we will accept
   as legitimate constants for TARGET_LEGITIMATE_CONSTANT_P.  These are
   initialized in override_options.  */
REAL_VALUE_TYPE dfhigh, dflow, sfhigh, sflow;

/* Array giving truth value on whether or not a given hard register
   can support a given mode.  */
char microblaze_hard_regno_mode_ok[(int)MAX_MACHINE_MODE]
				  [FIRST_PSEUDO_REGISTER];

/* Current frame information calculated by compute_frame_size.  */
struct microblaze_frame_info current_frame_info;

/* Zero structure to initialize current_frame_info.  */
struct microblaze_frame_info zero_frame_info;

/* List of all MICROBLAZE punctuation characters used by print_operand.  */
char microblaze_print_operand_punct[256];

/* Map GCC register number to debugger register number.  */
int microblaze_dbx_regno[FIRST_PSEUDO_REGISTER];

/* Map hard register number to register class.  */
enum reg_class microblaze_regno_to_class[] =
{
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  ST_REGS,	GR_REGS,	GR_REGS,	GR_REGS
};

/* MicroBlaze specific machine attributes.
   interrupt_handler - Interrupt handler attribute to add interrupt prologue 
		       and epilogue and use appropriate interrupt return.
   save_volatiles    - Similar to interrupt handler, but use normal return.  */
int interrupt_handler;
int save_volatiles;

const struct attribute_spec microblaze_attribute_table[] = {
  /* name         min_len, max_len, decl_req, type_req, fn_type, req_handler,
     affects_type_identity */
  {"interrupt_handler", 0,       0,     true,    false,   false,        NULL,
    false },
  {"save_volatiles"   , 0,       0,     true,    false,   false,        NULL,
    false },
  { NULL,        	0,       0,    false,    false,   false,        NULL,
    false }
};

static int microblaze_interrupt_function_p (tree);

section *sdata2_section;

/* Return truth value if a CONST_DOUBLE is ok to be a legitimate constant.  */
static bool
microblaze_const_double_ok (rtx op, enum machine_mode mode)
{
  REAL_VALUE_TYPE d;

  if (GET_CODE (op) != CONST_DOUBLE)
    return 0;

  if (GET_MODE (op) == VOIDmode)
    return 1;

  if (mode != SFmode && mode != DFmode)
    return 0;

  if (op == CONST0_RTX (mode))
    return 1;

  REAL_VALUE_FROM_CONST_DOUBLE (d, op);

  if (REAL_VALUE_ISNAN (d))
    return FALSE;

  if (REAL_VALUE_NEGATIVE (d))
    d = real_value_negate (&d);

  if (mode == DFmode)
    {
      if (REAL_VALUES_LESS (d, dfhigh) && REAL_VALUES_LESS (dflow, d))
	return 1;
    }
  else
    {
      if (REAL_VALUES_LESS (d, sfhigh) && REAL_VALUES_LESS (sflow, d))
	return 1;
    }

  return 0;
}

/* Return truth value if a memory operand fits in a single instruction
   (ie, register + small offset) or (register + register).  */

int
simple_memory_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  rtx addr, plus0, plus1;

  /* Eliminate non-memory operations.  */
  if (GET_CODE (op) != MEM)
    return 0;

  /* dword operations really put out 2 instructions, so eliminate them.  */
  /* ??? This isn't strictly correct.  It is OK to accept multiword modes
     here, since the length attributes are being set correctly, but only
     if the address is offsettable.  */
  if (GET_MODE_SIZE (GET_MODE (op)) > UNITS_PER_WORD)
    return 0;


  /* Decode the address now.  */
  addr = XEXP (op, 0);
  switch (GET_CODE (addr))

    {
    case REG:
      return 1;

    case PLUS:
      plus0 = XEXP (addr, 0);
      plus1 = XEXP (addr, 1);

      if (GET_CODE (plus0) == REG && GET_CODE (plus1) == CONST_INT
	  && SMALL_INT (plus1))
	{
	  return 1;
	}
      else if (GET_CODE (plus1) == REG && GET_CODE (plus0) == CONST_INT)
	{
	  return 1;
	}
      else if (GET_CODE (plus0) == REG && GET_CODE (plus1) == REG)
	{
	  return 1;
	}
      else
	return 0;

    case SYMBOL_REF:
      return 0;

    default:
      break;
    }

  return 0;
}

/* Return nonzero for a memory address that can be used to load or store
   a doubleword.  */

int
double_memory_operand (rtx op, enum machine_mode mode)
{
  rtx addr;

  if (GET_CODE (op) != MEM || !memory_operand (op, mode))
    {
      /* During reload, we accept a pseudo register if it has an
         appropriate memory address.  If we don't do this, we will
         wind up reloading into a register, and then reloading that
         register from memory, when we could just reload directly from
         memory.  */
      if (reload_in_progress
	  && GET_CODE (op) == REG
	  && REGNO (op) >= FIRST_PSEUDO_REGISTER
	  && reg_renumber[REGNO (op)] < 0
	  && reg_equiv_mem (REGNO (op)) != 0
	  && double_memory_operand (reg_equiv_mem (REGNO (op)), mode))
	return 1;
      return 0;
    }

  /* Make sure that 4 added to the address is a valid memory address.
     This essentially just checks for overflow in an added constant.  */

  addr = XEXP (op, 0);

  if (CONSTANT_ADDRESS_P (addr))
    return 1;

  return memory_address_p ((GET_MODE_CLASS (mode) == MODE_INT
			    ? SImode : SFmode),
			   plus_constant (Pmode, addr, 4));
}

/* Implement REG_OK_FOR_BASE_P -and- REG_OK_FOR_INDEX_P.  */
int
microblaze_regno_ok_for_base_p (int regno, int strict)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (!strict)
	return true;
      regno = reg_renumber[regno];
    }

  /* These fake registers will be eliminated to either the stack or
     hard frame pointer, both of which are usually valid base registers.
     Reload deals with the cases where the eliminated form isn't valid.  */
  if (regno == ARG_POINTER_REGNUM || regno == FRAME_POINTER_REGNUM)
    return true;

  return GP_REG_P (regno);
}

/* Return true if X is a valid base register for the given mode.
   Allow only hard registers if STRICT.  */

static bool
microblaze_valid_base_register_p (rtx x,
				  enum machine_mode mode ATTRIBUTE_UNUSED,
				  int strict)
{
  if (!strict && GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  return (GET_CODE (x) == REG
	  && microblaze_regno_ok_for_base_p (REGNO (x), strict));
}

static bool
microblaze_classify_unspec (struct microblaze_address_info *info, rtx x)
{
  info->symbol_type = SYMBOL_TYPE_GENERAL;
  info->symbol = XVECEXP (x, 0, 0);

  if (XINT (x, 1) == UNSPEC_GOTOFF)
    {
      info->regA = gen_rtx_REG (SImode, PIC_OFFSET_TABLE_REGNUM);
      info->type = ADDRESS_GOTOFF;
    }
  else if (XINT (x, 1) == UNSPEC_PLT)
    {
      info->type = ADDRESS_PLT;
    }
  else
    {
      return false;
    }
  return true;
}


/* Return true if X is a valid index register for the given mode.
   Allow only hard registers if STRICT.  */

static bool
microblaze_valid_index_register_p (rtx x,
				   enum machine_mode mode ATTRIBUTE_UNUSED,
				   int strict)
{
  if (!strict && GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  return (GET_CODE (x) == REG
	  /* A base register is good enough to be an index register on MicroBlaze.  */
	  && microblaze_regno_ok_for_base_p (REGNO (x), strict));
}

/* Get the base register for accessing a value from the memory or
   Symbol ref. Used for MicroBlaze Small Data Area Pointer Optimization.  */
static int
get_base_reg (rtx x)
{
  tree decl;
  int base_reg = (flag_pic ? MB_ABI_PIC_ADDR_REGNUM : MB_ABI_BASE_REGNUM);

  if (TARGET_XLGPOPT
      && GET_CODE (x) == SYMBOL_REF
      && SYMBOL_REF_SMALL_P (x) && (decl = SYMBOL_REF_DECL (x)) != NULL)
    {
      if (TREE_READONLY (decl))
	base_reg = MB_ABI_GPRO_REGNUM;
      else
	base_reg = MB_ABI_GPRW_REGNUM;
    }

  return base_reg;
}

/* Return true if X is a valid address for machine mode MODE.  If it is,
   fill in INFO appropriately.  STRICT is true if we should only accept
   hard base registers.  

      type                     regA      regB    offset      symbol

   ADDRESS_INVALID             NULL      NULL     NULL        NULL

   ADDRESS_REG                 %0        NULL     const_0 /   NULL
                                                  const_int
   ADDRESS_REG_INDEX           %0        %1       NULL        NULL

   ADDRESS_SYMBOLIC            r0 /      NULL     NULL        symbol    
                           sda_base_reg 

   ADDRESS_CONST_INT           r0       NULL      const       NULL

   For modes spanning multiple registers (DFmode in 32-bit GPRs,
   DImode, TImode), indexed addressing cannot be used because
   adjacent memory cells are accessed by adding word-sized offsets
   during assembly output.  */

static bool
microblaze_classify_address (struct microblaze_address_info *info, rtx x,
			     enum machine_mode mode, int strict)
{
  rtx xplus0;
  rtx xplus1;

  info->type = ADDRESS_INVALID;
  info->regA = NULL;
  info->regB = NULL;
  info->offset = NULL;
  info->symbol = NULL;
  info->symbol_type = SYMBOL_TYPE_INVALID;

  switch (GET_CODE (x))
    {
    case REG:
    case SUBREG:
      {
	info->type = ADDRESS_REG;
	info->regA = x;
	info->offset = const0_rtx;
	return microblaze_valid_base_register_p (info->regA, mode, strict);
      }
    case PLUS:
      {
	xplus0 = XEXP (x, 0);
	xplus1 = XEXP (x, 1);

	if (microblaze_valid_base_register_p (xplus0, mode, strict))
	  {
	    info->type = ADDRESS_REG;
	    info->regA = xplus0;

	    if (GET_CODE (xplus1) == CONST_INT)
	      {
		info->offset = xplus1;
		return true;
	      }
	    else if (GET_CODE (xplus1) == UNSPEC)
	      {
		return microblaze_classify_unspec (info, xplus1);
	      }
	    else if ((GET_CODE (xplus1) == SYMBOL_REF ||
		      GET_CODE (xplus1) == LABEL_REF) && flag_pic == 2)
	      {
		return false;
	      }
	    else if (GET_CODE (xplus1) == SYMBOL_REF ||
		     GET_CODE (xplus1) == LABEL_REF ||
		     GET_CODE (xplus1) == CONST)
	      {
		if (GET_CODE (XEXP (xplus1, 0)) == UNSPEC)
		  return microblaze_classify_unspec (info, XEXP (xplus1, 0));
		else if (flag_pic == 2)
		  {
		    return false;
		  }
		info->type = ADDRESS_SYMBOLIC;
		info->symbol = xplus1;
		info->symbol_type = SYMBOL_TYPE_GENERAL;
		return true;
	      }
	    else if (GET_CODE (xplus1) == REG
		     && microblaze_valid_index_register_p (xplus1, mode,
							   strict)
		     && (GET_MODE_SIZE (mode) <= UNITS_PER_WORD))
	      {
		/* Restrict larger than word-width modes from using an index register.  */
		info->type = ADDRESS_REG_INDEX;
		info->regB = xplus1;
		return true;
	      }
	  }
	break;
      }
    case CONST_INT:
      {
	info->regA = gen_rtx_raw_REG (mode, 0);
	info->type = ADDRESS_CONST_INT;
	info->offset = x;
	return true;
      }
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      {
	info->type = ADDRESS_SYMBOLIC;
	info->symbol_type = SYMBOL_TYPE_GENERAL;
	info->symbol = x;
	info->regA = gen_rtx_raw_REG (mode, get_base_reg (x));

	if (GET_CODE (x) == CONST)
	  {
	    return !(flag_pic && pic_address_needs_scratch (x));
	  }
	else if (flag_pic == 2)
	  {
	    return false;
	  }

	return true;
      }

    case UNSPEC:
      {
	if (reload_in_progress)
	  df_set_regs_ever_live (PIC_OFFSET_TABLE_REGNUM, true);
	return microblaze_classify_unspec (info, x);
      }

    default:
      return false;
    }

  return false;
}

/* This function is used to implement GO_IF_LEGITIMATE_ADDRESS.  It
   returns a nonzero value if X is a legitimate address for a memory
   operand of the indicated MODE.  STRICT is nonzero if this function
   is called during reload.  */

bool
microblaze_legitimate_address_p (enum machine_mode mode, rtx x, bool strict)
{
  struct microblaze_address_info addr;

  return microblaze_classify_address (&addr, x, mode, strict);
}


/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This is used from only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was
   called.  In some cases it is useful to look at this to decide what
   needs to be done.

   It is always safe for this function to do nothing.  It exists to
   recognize opportunities to optimize the output.

   For the MicroBlaze, transform:

   memory(X + <large int>)

   into:

   Y = <large int> & ~0x7fff;
   Z = X + Y
   memory (Z + (<large int> & 0x7fff));

   This is for CSE to find several similar references, and only use one Z.

   When PIC, convert addresses of the form memory (symbol+large int) to
   memory (reg+large int).  */

static rtx
microblaze_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			       enum machine_mode mode ATTRIBUTE_UNUSED)
{
  register rtx xinsn = x, result;

  if (GET_CODE (xinsn) == CONST
      && flag_pic && pic_address_needs_scratch (xinsn))
    {
      rtx ptr_reg = gen_reg_rtx (Pmode);
      rtx constant = XEXP (XEXP (xinsn, 0), 1);

      emit_move_insn (ptr_reg, XEXP (XEXP (xinsn, 0), 0));

      result = gen_rtx_PLUS (Pmode, ptr_reg, constant);
      if (SMALL_INT (constant))
	return result;
      /* Otherwise we fall through so the code below will fix the 
         constant.  */
      xinsn = result;
    }

  if (GET_CODE (xinsn) == PLUS)
    {
      register rtx xplus0 = XEXP (xinsn, 0);
      register rtx xplus1 = XEXP (xinsn, 1);
      register enum rtx_code code0 = GET_CODE (xplus0);
      register enum rtx_code code1 = GET_CODE (xplus1);

      if (code0 != REG && code1 == REG)
	{
	  xplus0 = XEXP (xinsn, 1);
	  xplus1 = XEXP (xinsn, 0);
	  code0 = GET_CODE (xplus0);
	  code1 = GET_CODE (xplus1);
	}

      if (code0 == REG && REG_OK_FOR_BASE_P (xplus0)
	  && code1 == CONST_INT && !SMALL_INT (xplus1))
	{
	  rtx int_reg = gen_reg_rtx (Pmode);
	  rtx ptr_reg = gen_reg_rtx (Pmode);

	  emit_move_insn (int_reg, GEN_INT (INTVAL (xplus1) & ~0x7fff));

	  emit_insn (gen_rtx_SET (VOIDmode,
				  ptr_reg,
				  gen_rtx_PLUS (Pmode, xplus0, int_reg)));

	  result = gen_rtx_PLUS (Pmode, ptr_reg,
				 GEN_INT (INTVAL (xplus1) & 0x7fff));
	  return result;
	}

      if (code0 == REG && REG_OK_FOR_BASE_P (xplus0) && flag_pic == 2)
	{
	  if (reload_in_progress)
	    df_set_regs_ever_live (PIC_OFFSET_TABLE_REGNUM, true);
	  if (code1 == CONST)
	    {
	      xplus1 = XEXP (xplus1, 0);
	      code1 = GET_CODE (xplus1);
	    }
	  if (code1 == SYMBOL_REF)
	    {
	      result =
		gen_rtx_UNSPEC (Pmode, gen_rtvec (1, xplus1), UNSPEC_GOTOFF);
	      result = gen_rtx_CONST (Pmode, result);
	      result = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, result);
	      result = gen_const_mem (Pmode, result);
	      result = gen_rtx_PLUS (Pmode, xplus0, result);
	      return result;
	    }
	}
    }

  if (GET_CODE (xinsn) == SYMBOL_REF)
    {
      if (reload_in_progress)
	df_set_regs_ever_live (PIC_OFFSET_TABLE_REGNUM, true);
      result = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, xinsn), UNSPEC_GOTOFF);
      result = gen_rtx_CONST (Pmode, result);
      result = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, result);
      result = gen_const_mem (Pmode, result);
      return result;
    }

  return x;
}

/* Block Moves.  */

#define MAX_MOVE_REGS 8
#define MAX_MOVE_BYTES (MAX_MOVE_REGS * UNITS_PER_WORD)

/* Emit straight-line code to move LENGTH bytes from SRC to DEST.
   Assume that the areas do not overlap.  */

static void
microblaze_block_move_straight (rtx dest, rtx src, HOST_WIDE_INT length)
{
  HOST_WIDE_INT offset, delta;
  unsigned HOST_WIDE_INT bits;
  int i;
  enum machine_mode mode;
  rtx *regs;

  bits = BITS_PER_WORD;
  mode = mode_for_size (bits, MODE_INT, 0);
  delta = bits / BITS_PER_UNIT;

  /* Allocate a buffer for the temporary registers.  */
  regs = XALLOCAVEC (rtx, length / delta);

  /* Load as many BITS-sized chunks as possible.  Use a normal load if
     the source has enough alignment, otherwise use left/right pairs.  */
  for (offset = 0, i = 0; offset + delta <= length; offset += delta, i++)
    {
      regs[i] = gen_reg_rtx (mode);
      emit_move_insn (regs[i], adjust_address (src, mode, offset));
    }

  /* Copy the chunks to the destination.  */
  for (offset = 0, i = 0; offset + delta <= length; offset += delta, i++)
    emit_move_insn (adjust_address (dest, mode, offset), regs[i]);

  /* Mop up any left-over bytes.  */
  if (offset < length)
    {
      src = adjust_address (src, BLKmode, offset);
      dest = adjust_address (dest, BLKmode, offset);
      move_by_pieces (dest, src, length - offset,
		      MIN (MEM_ALIGN (src), MEM_ALIGN (dest)), 0);
    }
}

/* Helper function for doing a loop-based block operation on memory
   reference MEM.  Each iteration of the loop will operate on LENGTH
   bytes of MEM.

   Create a new base register for use within the loop and point it to
   the start of MEM.  Create a new memory reference that uses this
   register.  Store them in *LOOP_REG and *LOOP_MEM respectively.  */

static void
microblaze_adjust_block_mem (rtx mem, HOST_WIDE_INT length,
			     rtx * loop_reg, rtx * loop_mem)
{
  *loop_reg = copy_addr_to_reg (XEXP (mem, 0));

  /* Although the new mem does not refer to a known location,
     it does keep up to LENGTH bytes of alignment.  */
  *loop_mem = change_address (mem, BLKmode, *loop_reg);
  set_mem_align (*loop_mem,
		 MIN ((HOST_WIDE_INT) MEM_ALIGN (mem),
		      length * BITS_PER_UNIT));
}


/* Move LENGTH bytes from SRC to DEST using a loop that moves MAX_MOVE_BYTES
   per iteration.  LENGTH must be at least MAX_MOVE_BYTES.  Assume that the
   memory regions do not overlap.  */

static void
microblaze_block_move_loop (rtx dest, rtx src, HOST_WIDE_INT length)
{
  rtx label, src_reg, dest_reg, final_src;
  HOST_WIDE_INT leftover;

  leftover = length % MAX_MOVE_BYTES;
  length -= leftover;

  /* Create registers and memory references for use within the loop.  */
  microblaze_adjust_block_mem (src, MAX_MOVE_BYTES, &src_reg, &src);
  microblaze_adjust_block_mem (dest, MAX_MOVE_BYTES, &dest_reg, &dest);

  /* Calculate the value that SRC_REG should have after the last iteration
     of the loop.  */
  final_src = expand_simple_binop (Pmode, PLUS, src_reg, GEN_INT (length),
				   0, 0, OPTAB_WIDEN);

  /* Emit the start of the loop.  */
  label = gen_label_rtx ();
  emit_label (label);

  /* Emit the loop body.  */
  microblaze_block_move_straight (dest, src, MAX_MOVE_BYTES);

  /* Move on to the next block.  */
  emit_move_insn (src_reg, plus_constant (Pmode, src_reg, MAX_MOVE_BYTES));
  emit_move_insn (dest_reg, plus_constant (Pmode, dest_reg, MAX_MOVE_BYTES));

  /* Emit the test & branch.  */
  emit_insn (gen_cbranchsi4 (gen_rtx_NE (SImode, src_reg, final_src),
			     src_reg, final_src, label));

  /* Mop up any left-over bytes.  */
  if (leftover)
    microblaze_block_move_straight (dest, src, leftover);
}

/* Expand a movmemsi instruction.  */

bool
microblaze_expand_block_move (rtx dest, rtx src, rtx length, rtx align_rtx)
{

  if (GET_CODE (length) == CONST_INT)
    {
      HOST_WIDE_INT bytes = INTVAL (length);
      int align = INTVAL (align_rtx);

      if (align > UNITS_PER_WORD)
	{
	  align = UNITS_PER_WORD;	/* We can't do any better.  */
	}
      else if (align < UNITS_PER_WORD)
	{
	  if (INTVAL (length) <= MAX_MOVE_BYTES)
	    {
	      move_by_pieces (dest, src, bytes, align, 0);
	      return true;
	    }
	  else
	    return false;
	}

      if (INTVAL (length) <= 2 * MAX_MOVE_BYTES)
	{
	  microblaze_block_move_straight (dest, src, INTVAL (length));
	  return true;
	}
      else if (optimize)
	{
	  microblaze_block_move_loop (dest, src, INTVAL (length));
	  return true;
	}
    }
  return false;
}

static bool
microblaze_rtx_costs (rtx x, int code, int outer_code ATTRIBUTE_UNUSED,
		      int opno ATTRIBUTE_UNUSED, int *total,
		      bool speed ATTRIBUTE_UNUSED)
{
  enum machine_mode mode = GET_MODE (x);

  switch (code)
    {
    case MEM:
      {
	int num_words = (GET_MODE_SIZE (mode) > UNITS_PER_WORD) ? 2 : 1;
	if (simple_memory_operand (x, mode))
	  *total = COSTS_N_INSNS (2 * num_words);
	else
	  *total = COSTS_N_INSNS (2 * (2 * num_words));

	return true;
      }
    case NOT:
      {
	if (mode == DImode)
	  {
	    *total = COSTS_N_INSNS (2);
	  }
	else
	  *total = COSTS_N_INSNS (1);
	return false;
      }
    case AND:
    case IOR:
    case XOR:
      {
	if (mode == DImode)
	  {
	    *total = COSTS_N_INSNS (2);
	  }
	else
	  *total = COSTS_N_INSNS (1);

	return false;
      }
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      {
	if (TARGET_BARREL_SHIFT)
	  {
	    if (MICROBLAZE_VERSION_COMPARE (microblaze_select_cpu, "v5.00.a")
		>= 0)
	      *total = COSTS_N_INSNS (1);
	    else
	      *total = COSTS_N_INSNS (2);
	  }
	else if (!TARGET_SOFT_MUL)
	  *total = COSTS_N_INSNS (1);
	else if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	  {
	    /* Add 1 to make shift slightly more expensive than add.  */
	    *total = COSTS_N_INSNS (INTVAL (XEXP (x, 1))) + 1;
	    /* Reduce shift costs for special circumstances.  */
	    if (optimize_size && INTVAL (XEXP (x, 1)) > 5)
	      *total -= 2;
	    if (!optimize_size && INTVAL (XEXP (x, 1)) > 17)
	      *total -= 2;
	  }
	else
	  /* Double the worst cost of shifts when there is no barrel shifter and 
	     the shift amount is in a reg.  */
	  *total = COSTS_N_INSNS (32 * 4);
	return true;
      }
    case PLUS:
    case MINUS:
      {
	if (mode == SFmode || mode == DFmode)
	  {
	    if (TARGET_HARD_FLOAT)
	      *total = COSTS_N_INSNS (6);
	    return true;
	  }
	else if (mode == DImode)
	  {
	    *total = COSTS_N_INSNS (4);
	    return true;
	  }
	else
	  {
	    *total = COSTS_N_INSNS (1);
	    return true;
	  }

	return false;
      }
    case NEG:
      {
	if (mode == DImode)
	  *total = COSTS_N_INSNS (4);

	return false;
      }
    case MULT:
      {
	if (mode == SFmode)
	  {
	    if (TARGET_HARD_FLOAT)
	      *total = COSTS_N_INSNS (6);
	  }
	else if (!TARGET_SOFT_MUL)
	  {
	    if (MICROBLAZE_VERSION_COMPARE (microblaze_select_cpu, "v5.00.a")
		>= 0)
	      *total = COSTS_N_INSNS (1);
	    else
	      *total = COSTS_N_INSNS (3);
	  }
	else
	  *total = COSTS_N_INSNS (10);
	return true;
      }
    case DIV:
    case UDIV:
      {
	if (mode == SFmode)
	  {
	    if (TARGET_HARD_FLOAT)
	      *total = COSTS_N_INSNS (23);
	  }
	return false;
      }
    case SIGN_EXTEND:
      {
	*total = COSTS_N_INSNS (1);
	return false;
      }
    case ZERO_EXTEND:
      {
	*total = COSTS_N_INSNS (1);
	return false;
      }
    }

  return false;
}

/* Return the number of instructions needed to load or store a value
   of mode MODE at X.  Return 0 if X isn't valid for MODE.  */

static int
microblaze_address_insns (rtx x, enum machine_mode mode)
{
  struct microblaze_address_info addr;

  if (microblaze_classify_address (&addr, x, mode, false))
    {
      switch (addr.type)
	{
	case ADDRESS_REG:
	  if (SMALL_INT (addr.offset))
	    return 1;
	  else
	    return 2;
	case ADDRESS_CONST_INT:
	  if (SMALL_INT (x))
	    return 1;
	  else
	    return 2;
	case ADDRESS_REG_INDEX:
	case ADDRESS_SYMBOLIC:
	  return 1;
	case ADDRESS_GOTOFF:
	  return 2;
	default:
	  break;
	}
    }
  return 0;
}

/* Provide the costs of an addressing mode that contains ADDR.
   If ADDR is not a valid address, its cost is irrelevant.  */
static int
microblaze_address_cost (rtx addr, bool speed ATTRIBUTE_UNUSED)
{
  return COSTS_N_INSNS (microblaze_address_insns (addr, GET_MODE (addr)));
}

/* Return nonzero if X is an address which needs a temporary register when 
   reloaded while generating PIC code.  */

int
pic_address_needs_scratch (rtx x)
{
  /* An address which is a symbolic plus a non SMALL_INT needs a temp reg.  */
  if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
      && (flag_pic == 2 || !SMALL_INT (XEXP (XEXP (x, 0), 1))))
    return 1;

  return 0;
}

/* Argument support functions.  */
/* Initialize CUMULATIVE_ARGS for a function.  */

void
init_cumulative_args (CUMULATIVE_ARGS * cum, tree fntype,
		      rtx libname ATTRIBUTE_UNUSED)
{
  static CUMULATIVE_ARGS zero_cum;
  tree param, next_param;

  *cum = zero_cum;

  /* Determine if this function has variable arguments.  This is
     indicated by the last argument being 'void_type_mode' if there
     are no variable arguments.  The standard MicroBlaze calling sequence
     passes all arguments in the general purpose registers in this case. */

  for (param = fntype ? TYPE_ARG_TYPES (fntype) : 0;
       param != 0; param = next_param)
    {
      next_param = TREE_CHAIN (param);
      if (next_param == 0 && TREE_VALUE (param) != void_type_node)
	cum->gp_reg_found = 1;
    }
}

/* Advance the argument to the next argument position.  */

static void
microblaze_function_arg_advance (cumulative_args_t cum_v,
				 enum machine_mode mode,
				 const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  cum->arg_number++;
  switch (mode)
    {
    case VOIDmode:
      break;

    default:
      gcc_assert (GET_MODE_CLASS (mode) == MODE_COMPLEX_INT
	  || GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT);

      cum->gp_reg_found = 1;
      cum->arg_words += ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1)
			 / UNITS_PER_WORD);
      break;

    case BLKmode:
      cum->gp_reg_found = 1;
      cum->arg_words += ((int_size_in_bytes (type) + UNITS_PER_WORD - 1)
			 / UNITS_PER_WORD);
      break;

    case SFmode:
      cum->arg_words++;
      if (!cum->gp_reg_found && cum->arg_number <= 2)
	cum->fp_code += 1 << ((cum->arg_number - 1) * 2);
      break;

    case DFmode:
      cum->arg_words += 2;
      if (!cum->gp_reg_found && cum->arg_number <= 2)
	cum->fp_code += 2 << ((cum->arg_number - 1) * 2);
      break;

    case DImode:
      cum->gp_reg_found = 1;
      cum->arg_words += 2;
      break;

    case QImode:
    case HImode:
    case SImode:
    case TImode:
      cum->gp_reg_found = 1;
      cum->arg_words++;
      break;
    }
}

/* Return an RTL expression containing the register for the given mode,
   or 0 if the argument is to be passed on the stack.  */

static rtx
microblaze_function_arg (cumulative_args_t cum_v, enum machine_mode mode, 
			 const_tree type ATTRIBUTE_UNUSED,
			 bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  rtx ret;
  int regbase = -1;
  int *arg_words = &cum->arg_words;

  cum->last_arg_fp = 0;
  switch (mode)
    {
    case SFmode:
    case DFmode:
    case VOIDmode:
    case QImode:
    case HImode:
    case SImode:
    case DImode:
    case TImode:
      regbase = GP_ARG_FIRST;
      break;
    default:
      gcc_assert (GET_MODE_CLASS (mode) == MODE_COMPLEX_INT
	  || GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT);
      /* Drops through.  */
    case BLKmode:
      regbase = GP_ARG_FIRST;
      break;
    }

  if (*arg_words >= MAX_ARGS_IN_REGISTERS)
    ret = 0;
  else
    {
      gcc_assert (regbase != -1);

      ret = gen_rtx_REG (mode, regbase + *arg_words);
    }

  if (mode == VOIDmode)
    {
      if (cum->num_adjusts > 0)
	ret = gen_rtx_PARALLEL ((enum machine_mode) cum->fp_code,
				gen_rtvec_v (cum->num_adjusts, cum->adjust));
    }

  return ret;
}

/* Return number of bytes of argument to put in registers. */
static int
function_arg_partial_bytes (cumulative_args_t cum_v, enum machine_mode mode,	
			    tree type, bool named ATTRIBUTE_UNUSED)	
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if ((mode == BLKmode
       || GET_MODE_CLASS (mode) != MODE_COMPLEX_INT
       || GET_MODE_CLASS (mode) != MODE_COMPLEX_FLOAT)
      && cum->arg_words < MAX_ARGS_IN_REGISTERS)
    {
      int words;
      if (mode == BLKmode)
	words = ((int_size_in_bytes (type) + UNITS_PER_WORD - 1)
		 / UNITS_PER_WORD);
      else
	words = (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

      if (words + cum->arg_words <= MAX_ARGS_IN_REGISTERS)
	return 0;		/* structure fits in registers */

      return (MAX_ARGS_IN_REGISTERS - cum->arg_words) * UNITS_PER_WORD;
    }

  else if (mode == DImode && cum->arg_words == MAX_ARGS_IN_REGISTERS - 1)
    return UNITS_PER_WORD;

  return 0;
}

/*  Convert a version number of the form "vX.YY.Z" to an integer encoding 
    for easier range comparison.  */
static int
microblaze_version_to_int (const char *version)
{
  const char *p, *v;
  const char *tmpl = "vX.YY.Z";
  int iver = 0;

  p = version;
  v = tmpl;

  while (*v)
    {
      if (*v == 'X')
	{			/* Looking for major  */
	  if (!(*p >= '0' && *p <= '9'))
	    return -1;
	  iver += (int) (*p - '0');
	  iver *= 10;
	}
      else if (*v == 'Y')
	{			/* Looking for minor  */
	  if (!(*p >= '0' && *p <= '9'))
	    return -1;
	  iver += (int) (*p - '0');
	  iver *= 10;
	}
      else if (*v == 'Z')
	{			/* Looking for compat  */
	  if (!(*p >= 'a' && *p <= 'z'))
	    return -1;
	  iver *= 10;
	  iver += (int) (*p - 'a');
	}
      else
	{
	  if (*p != *v)
	    return -1;
	}

      v++;
      p++;
    }

  if (*p)
    return -1;

  return iver;
}


static void
microblaze_option_override (void)
{
  register int i, start;
  register int regno;
  register enum machine_mode mode;
  int ver;

  microblaze_section_threshold = (global_options_set.x_g_switch_value
				  ? g_switch_value
				  : MICROBLAZE_DEFAULT_GVALUE);

  /* Check the MicroBlaze CPU version for any special action to be done.  */
  if (microblaze_select_cpu == NULL)
    microblaze_select_cpu = MICROBLAZE_DEFAULT_CPU;
  ver = microblaze_version_to_int (microblaze_select_cpu);
  if (ver == -1)
    {
      error ("%qs is an invalid argument to -mcpu=", microblaze_select_cpu);
    }

  ver = MICROBLAZE_VERSION_COMPARE (microblaze_select_cpu, "v3.00.a");
  if (ver < 0)
    {
      /* No hardware exceptions in earlier versions. So no worries.  */
#if 0
      microblaze_select_flags &= ~(MICROBLAZE_MASK_NO_UNSAFE_DELAY);
#endif
      microblaze_no_unsafe_delay = 0;
      microblaze_pipe = MICROBLAZE_PIPE_3;
    }
  else if (ver == 0
	   || (MICROBLAZE_VERSION_COMPARE (microblaze_select_cpu, "v4.00.b")
	       == 0))
    {
#if 0
      microblaze_select_flags |= (MICROBLAZE_MASK_NO_UNSAFE_DELAY);
#endif
      microblaze_no_unsafe_delay = 1;
      microblaze_pipe = MICROBLAZE_PIPE_3;
    }
  else
    {
      /* We agree to use 5 pipe-stage model even on area optimized 3 
         pipe-stage variants.  */
#if 0
      microblaze_select_flags &= ~(MICROBLAZE_MASK_NO_UNSAFE_DELAY);
#endif
      microblaze_no_unsafe_delay = 0;
      microblaze_pipe = MICROBLAZE_PIPE_5;
      if (MICROBLAZE_VERSION_COMPARE (microblaze_select_cpu, "v5.00.a") == 0
	  || MICROBLAZE_VERSION_COMPARE (microblaze_select_cpu,
					 "v5.00.b") == 0
	  || MICROBLAZE_VERSION_COMPARE (microblaze_select_cpu,
					 "v5.00.c") == 0)
	{
	  /* Pattern compares are to be turned on by default only when 
 	     compiling for MB v5.00.'z'.  */
	  target_flags |= MASK_PATTERN_COMPARE;
	}
    }

  ver = MICROBLAZE_VERSION_COMPARE (microblaze_select_cpu, "v6.00.a");
  if (ver < 0)
    {
      if (TARGET_MULTIPLY_HIGH)
	warning (0,
		 "-mxl-multiply-high can be used only with -mcpu=v6.00.a or greater");
    }

  if (TARGET_MULTIPLY_HIGH && TARGET_SOFT_MUL)
    error ("-mxl-multiply-high requires -mno-xl-soft-mul");

  /* Always use DFA scheduler.  */
  microblaze_sched_use_dfa = 1;

#if 0
  microblaze_abicalls = MICROBLAZE_ABICALLS_NO;
#endif

  /* Initialize the high, low values for legit floating point constants.  */
  real_maxval (&dfhigh, 0, DFmode);
  real_maxval (&dflow, 1, DFmode);
  real_maxval (&sfhigh, 0, SFmode);
  real_maxval (&sflow, 1, SFmode);

  microblaze_print_operand_punct['?'] = 1;
  microblaze_print_operand_punct['#'] = 1;
  microblaze_print_operand_punct['&'] = 1;
  microblaze_print_operand_punct['!'] = 1;
  microblaze_print_operand_punct['*'] = 1;
  microblaze_print_operand_punct['@'] = 1;
  microblaze_print_operand_punct['.'] = 1;
  microblaze_print_operand_punct['('] = 1;
  microblaze_print_operand_punct[')'] = 1;
  microblaze_print_operand_punct['['] = 1;
  microblaze_print_operand_punct[']'] = 1;
  microblaze_print_operand_punct['<'] = 1;
  microblaze_print_operand_punct['>'] = 1;
  microblaze_print_operand_punct['{'] = 1;
  microblaze_print_operand_punct['}'] = 1;
  microblaze_print_operand_punct['^'] = 1;
  microblaze_print_operand_punct['$'] = 1;
  microblaze_print_operand_punct['+'] = 1;

  /* Set up array to map GCC register number to debug register number.
     Ignore the special purpose register numbers.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    microblaze_dbx_regno[i] = -1;

  start = GP_DBX_FIRST - GP_REG_FIRST;
  for (i = GP_REG_FIRST; i <= GP_REG_LAST; i++)
    microblaze_dbx_regno[i] = i + start;

  /* Set up array giving whether a given register can hold a given mode.   */

  for (mode = VOIDmode;
       mode != MAX_MACHINE_MODE; mode = (enum machine_mode) ((int) mode + 1))
    {
      register int size = GET_MODE_SIZE (mode);

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	{
	  register int ok;

	  if (mode == CCmode)
	    {
	      ok = (ST_REG_P (regno) || GP_REG_P (regno));
	    }
	  else if (GP_REG_P (regno))
	    ok = ((regno & 1) == 0 || size <= UNITS_PER_WORD);
	  else
	    ok = 0;

	  microblaze_hard_regno_mode_ok[(int) mode][regno] = ok;
	}
    }
}

/* Return true if FUNC is an interrupt function as specified
   by the "interrupt_handler" attribute.  */

static int
microblaze_interrupt_function_p (tree func)
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    return 0;

  a = lookup_attribute ("interrupt_handler", DECL_ATTRIBUTES (func));
  return a != NULL_TREE;
}

/* Return true if FUNC is an interrupt function which uses
   normal return, indicated by the "save_volatiles" attribute.  */

static int
microblaze_save_volatiles (tree func)
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    return 0;

  a = lookup_attribute ("save_volatiles", DECL_ATTRIBUTES (func));
  return a != NULL_TREE;
}

/* Return whether function is tagged with 'interrupt_handler'
   attribute.  Return true if function should use return from
   interrupt rather than normal function return.  */
int
microblaze_is_interrupt_handler (void)
{
  return interrupt_handler;
}

/* Determine of register must be saved/restored in call.  */
static int
microblaze_must_save_register (int regno)
{
  if (pic_offset_table_rtx &&
      (regno == MB_ABI_PIC_ADDR_REGNUM) && df_regs_ever_live_p (regno))
    return 1;

  if (df_regs_ever_live_p (regno) && !call_used_regs[regno])
    return 1;

  if (frame_pointer_needed && (regno == HARD_FRAME_POINTER_REGNUM))
    return 1;

  if (!crtl->is_leaf)
    {
      if (regno == MB_ABI_SUB_RETURN_ADDR_REGNUM)
	return 1;
      if ((interrupt_handler || save_volatiles) &&
	  (regno >= 3 && regno <= 12))
	return 1;
    }

  if (interrupt_handler)
    {
      if (df_regs_ever_live_p (regno) 
	  || regno == MB_ABI_MSR_SAVE_REG
	  || regno == MB_ABI_ASM_TEMP_REGNUM
	  || regno == MB_ABI_EXCEPTION_RETURN_ADDR_REGNUM)
	return 1;
    }

  if (save_volatiles)
    {
      if (df_regs_ever_live_p (regno)
	  || regno == MB_ABI_ASM_TEMP_REGNUM
	  || regno == MB_ABI_EXCEPTION_RETURN_ADDR_REGNUM)
	return 1;
    }

  return 0;
}

/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.

   MicroBlaze stack frames look like:



             Before call		        After call
        +-----------------------+	+-----------------------+
   high |			|       |      			|
   mem. |  local variables,     |	|  local variables,	|
        |  callee saved and     |       |  callee saved and    	|
	|  temps     		|       |  temps     	        |
        +-----------------------+	+-----------------------+
        |  arguments for called	|       |  arguments for called |
	|  subroutines		|	|  subroutines  	|
        |  (optional)           |       |  (optional)           |
        +-----------------------+	+-----------------------+
	|  Link register 	|	|  Link register        |
    SP->|                       |       |                       |
	+-----------------------+       +-----------------------+
					|		        |
                                        |  local variables,     |
                                        |  callee saved and     |
                                        |  temps                |
					+-----------------------+
                                        |   MSR (optional if,   |
                                        |   interrupt handler)  |
					+-----------------------+
					|			|
                                        |  alloca allocations   |
        				|			|
					+-----------------------+
					|			|
                                        |  arguments for called |
                                        |  subroutines          |
                                        |  (optional)           |
        				|		        |
					+-----------------------+
                                        |  Link register        |
   low                           FP,SP->|                       |
   memory        			+-----------------------+

*/

static HOST_WIDE_INT
compute_frame_size (HOST_WIDE_INT size)	
{
  int regno;
  HOST_WIDE_INT total_size;	/* # bytes that the entire frame takes up.  */
  HOST_WIDE_INT var_size;	/* # bytes that local variables take up.  */
  HOST_WIDE_INT args_size;	/* # bytes that outgoing arguments take up.  */
  int link_debug_size;		/* # bytes for link register.  */
  HOST_WIDE_INT gp_reg_size;	/* # bytes needed to store calle-saved gp regs.  */
  long mask;			/* mask of saved gp registers.  */

  interrupt_handler =
    microblaze_interrupt_function_p (current_function_decl);
  save_volatiles = microblaze_save_volatiles (current_function_decl);

  gp_reg_size = 0;
  mask = 0;
  var_size = size;
  args_size = crtl->outgoing_args_size;

  if ((args_size == 0) && cfun->calls_alloca)
    args_size = NUM_OF_ARGS * UNITS_PER_WORD;

  total_size = var_size + args_size;

  if (flag_pic == 2)
    /* force setting GOT.  */
    df_set_regs_ever_live (MB_ABI_PIC_ADDR_REGNUM, true);

  /* Calculate space needed for gp registers.  */
  for (regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    {
      if (microblaze_must_save_register (regno))
	{

	  if (regno != MB_ABI_SUB_RETURN_ADDR_REGNUM)
	    /* Don't account for link register. It is accounted specially below.  */
	    gp_reg_size += GET_MODE_SIZE (SImode);

	  mask |= (1L << (regno - GP_REG_FIRST));
	}
    }

  total_size += gp_reg_size;

  /* Add 4 bytes for MSR.  */
  if (interrupt_handler)
    total_size += 4;

  /* No space to be allocated for link register in leaf functions with no other
     stack requirements.  */
  if (total_size == 0 && crtl->is_leaf)
    link_debug_size = 0;
  else
    link_debug_size = UNITS_PER_WORD;

  total_size += link_debug_size;

  /* Save other computed information.  */
  current_frame_info.total_size = total_size;
  current_frame_info.var_size = var_size;
  current_frame_info.args_size = args_size;
  current_frame_info.gp_reg_size = gp_reg_size;
  current_frame_info.mask = mask;
  current_frame_info.initialized = reload_completed;
  current_frame_info.num_gp = gp_reg_size / UNITS_PER_WORD;
  current_frame_info.link_debug_size = link_debug_size;

  if (mask)
    /* Offset from which to callee-save GP regs.  */
    current_frame_info.gp_offset = (total_size - gp_reg_size);
  else
    current_frame_info.gp_offset = 0;

  /* Ok, we're done.  */
  return total_size;
}

/* Make sure that we're not trying to eliminate to the wrong hard frame
   pointer.  */

static bool
microblaze_can_eliminate (const int from, const int to)
{
  return ((from == RETURN_ADDRESS_POINTER_REGNUM && !leaf_function_p())
   	  || (to == MB_ABI_SUB_RETURN_ADDR_REGNUM && leaf_function_p())
  	  || (from != RETURN_ADDRESS_POINTER_REGNUM
   	      && (to == HARD_FRAME_POINTER_REGNUM
		  || (to == STACK_POINTER_REGNUM && !frame_pointer_needed))));
}

/* Implement INITIAL_ELIMINATION_OFFSET.  FROM is either the frame
   pointer or argument pointer or the return address pointer.  TO is either 
   the stack pointer or hard frame pointer.  */

HOST_WIDE_INT
microblaze_initial_elimination_offset (int from, int to)
{
  HOST_WIDE_INT offset;

  switch (from)
    {
    case FRAME_POINTER_REGNUM:
      offset = 0;
      break;
    case ARG_POINTER_REGNUM:
      if (to == STACK_POINTER_REGNUM || to == HARD_FRAME_POINTER_REGNUM)
	offset = compute_frame_size (get_frame_size ());
      else
	gcc_unreachable ();
      break;
    case RETURN_ADDRESS_POINTER_REGNUM:
      if (crtl->is_leaf)
	offset = 0;
      else
	offset = current_frame_info.gp_offset +
	  ((UNITS_PER_WORD - (POINTER_SIZE / BITS_PER_UNIT)));
      break;
    default:
      gcc_unreachable ();
    }
  return offset;
}

/* Print operands using format code.
 
   The MicroBlaze specific codes are:

   'X'  X is CONST_INT, prints 32 bits in hexadecimal format = "0x%08x",
   'x'  X is CONST_INT, prints 16 bits in hexadecimal format = "0x%04x",
   'F'  op is CONST_DOUBLE, print 32 bits in hex,
   'd'  output integer constant in decimal,
   'z'	if the operand is 0, use $0 instead of normal operand.
   'D'  print second register of double-word register operand.
   'L'  print low-order register of double-word register operand.
   'M'  print high-order register of double-word register operand.
   'C'  print part of opcode for a branch condition.
   'N'  print part of opcode for a branch condition, inverted.
   'S'  X is CODE_LABEL, print with prefix of "LS" (for embedded switch).
   'B'  print 'z' for EQ, 'n' for NE
   'b'  print 'n' for EQ, 'z' for NE
   'T'  print 'f' for EQ, 't' for NE
   't'  print 't' for EQ, 'f' for NE
   'm'  Print 1<<operand.
   'i'  Print 'i' if MEM operand has immediate value
   'o'	Print operand address+4
   '?'	Print 'd' if we use a branch with delay slot instead of normal branch.
   'h'  Print high word of const_double (int or float) value as hex
   'j'  Print low word of const_double (int or float) value as hex
   's'  Print -1 if operand is negative, 0 if positive (sign extend)
   '@'	Print the name of the temporary register (rMB_ABI_ASM_TEMP_REGNUM).
   '#'	Print nop if the delay slot of a branch is not filled. 
*/

void
print_operand (FILE * file, rtx op, int letter)
{
  register enum rtx_code code;

  if (PRINT_OPERAND_PUNCT_VALID_P (letter))
    {
      switch (letter)
	{
	case '?':
	  /* Conditionally add a 'd' to indicate filled delay slot.  */
	  if (final_sequence != NULL)
	    fputs ("d", file);
	  break;

	case '#':
	  /* Conditionally add a nop in unfilled delay slot.  */
	  if (final_sequence == NULL)
	    fputs ("nop\t\t# Unfilled delay slot\n", file);
	  break;

	case '@':
	  fputs (reg_names[GP_REG_FIRST + MB_ABI_ASM_TEMP_REGNUM], file);
	  break;

	default:
	  output_operand_lossage ("unknown punctuation '%c'", letter);
	  break;
	}

      return;
    }

  if (!op)
    {
      output_operand_lossage ("null pointer");
      return;
    }

  code = GET_CODE (op);

  if (code == SIGN_EXTEND)
    op = XEXP (op, 0), code = GET_CODE (op);

  if (letter == 'C')
    switch (code)
      {
      case EQ:
	fputs ("eq", file);
	break;
      case NE:
	fputs ("ne", file);
	break;
      case GT:
      case GTU:
	fputs ("gt", file);
	break;
      case GE:
      case GEU:
	fputs ("ge", file);
	break;
      case LT:
      case LTU:
	fputs ("lt", file);
	break;
      case LE:
      case LEU:
	fputs ("le", file);
	break;
      default:
	fatal_insn ("PRINT_OPERAND, invalid insn for %%C", op);
      }

  else if (letter == 'N')
    switch (code)
      {
      case EQ:
	fputs ("ne", file);
	break;
      case NE:
	fputs ("eq", file);
	break;
      case GT:
      case GTU:
	fputs ("le", file);
	break;
      case GE:
      case GEU:
	fputs ("lt", file);
	break;
      case LT:
      case LTU:
	fputs ("ge", file);
	break;
      case LE:
      case LEU:
	fputs ("gt", file);
	break;
      default:
	fatal_insn ("PRINT_OPERAND, invalid insn for %%N", op);
      }

  else if (letter == 'S')
    {
      char buffer[100];

      ASM_GENERATE_INTERNAL_LABEL (buffer, "LS", CODE_LABEL_NUMBER (op));
      assemble_name (file, buffer);
    }

  /* Print 'i' for memory operands which have immediate values.  */
  else if (letter == 'i')
    {
      if (code == MEM)
	{
	  struct microblaze_address_info info;

	  if (!microblaze_classify_address
	      (&info, XEXP (op, 0), GET_MODE (op), 1))
	    fatal_insn ("insn contains an invalid address !", op);

	  switch (info.type)
	    {
	    case ADDRESS_REG:
	    case ADDRESS_CONST_INT:
	    case ADDRESS_SYMBOLIC:
	    case ADDRESS_GOTOFF:
	      fputs ("i", file);
	      break;
	    case ADDRESS_REG_INDEX:
	      break;
	    case ADDRESS_INVALID:
	    case ADDRESS_PLT:
	      fatal_insn ("invalid address", op);
	    }
	}
    }

  else if (code == REG || code == SUBREG)
    {
      register int regnum;

      if (code == REG)
	regnum = REGNO (op);
      else
	regnum = true_regnum (op);

      if ((letter == 'M' && !WORDS_BIG_ENDIAN)
	  || (letter == 'L' && WORDS_BIG_ENDIAN) || letter == 'D')
	regnum++;

      fprintf (file, "%s", reg_names[regnum]);
    }

  else if (code == MEM)
    if (letter == 'o')
      {
	rtx op4 = adjust_address (op, GET_MODE (op), 4);
	output_address (XEXP (op4, 0));
      }
    else
      output_address (XEXP (op, 0));

  else if (letter == 'h' || letter == 'j')
    {
      long val[2];
      if (code == CONST_DOUBLE)
	{
	  if (GET_MODE (op) == DFmode)
	    {
	      REAL_VALUE_TYPE value;
	      REAL_VALUE_FROM_CONST_DOUBLE (value, op);
	      REAL_VALUE_TO_TARGET_DOUBLE (value, val);
	    }
	  else
	    {
	      val[0] = CONST_DOUBLE_HIGH (op);
	      val[1] = CONST_DOUBLE_LOW (op);
	    }
	}
      else if (code == CONST_INT)
        {
	  val[0] = (INTVAL (op) & 0xffffffff00000000LL) >> 32;
	  val[1] = INTVAL (op) & 0x00000000ffffffffLL;
	  if (val[0] == 0 && val[1] < 0)
	    val[0] = -1;
	    
        }
      fprintf (file, "0x%8.8lx", (letter == 'h') ? val[0] : val[1]);
    }
  else if (code == CONST_DOUBLE)
    {
      if (letter == 'F')
	{
	  unsigned long value_long;
	  REAL_VALUE_TYPE value;
	  REAL_VALUE_FROM_CONST_DOUBLE (value, op);
	  REAL_VALUE_TO_TARGET_SINGLE (value, value_long);
	  fprintf (file, HOST_WIDE_INT_PRINT_HEX, value_long);
	}
      else
	{
	  char s[60];
	  real_to_decimal (s, CONST_DOUBLE_REAL_VALUE (op), sizeof (s), 0, 1);
	  fputs (s, file);
	}
    }

  else if (code == UNSPEC)
    {
      print_operand_address (file, op);
    }

  else if (letter == 'x' && GET_CODE (op) == CONST_INT)
    fprintf (file, HOST_WIDE_INT_PRINT_HEX, 0xffff & INTVAL (op));

  else if (letter == 'X' && GET_CODE (op) == CONST_INT)
    fprintf (file, HOST_WIDE_INT_PRINT_HEX, INTVAL (op));

  else if (letter == 'd' && GET_CODE (op) == CONST_INT)
    fprintf (file, HOST_WIDE_INT_PRINT_DEC, (INTVAL (op)));

  else if (letter == 'z' && GET_CODE (op) == CONST_INT && INTVAL (op) == 0)
    fputs (reg_names[GP_REG_FIRST], file);

  else if (letter == 's' && GET_CODE (op) == CONST_INT)
    if (INTVAL (op) < 0)
      fputs ("-1", file);
    else
      fputs ("0", file);

  else if (letter == 'd' || letter == 'x' || letter == 'X' || letter == 's')
    output_operand_lossage ("letter %c was found & insn was not CONST_INT", letter);

  else if (letter == 'B')
    fputs (code == EQ ? "z" : "n", file);
  else if (letter == 'b')
    fputs (code == EQ ? "n" : "z", file);
  else if (letter == 'T')
    fputs (code == EQ ? "f" : "t", file);
  else if (letter == 't')
    fputs (code == EQ ? "t" : "f", file);

  else if (code == CONST && GET_CODE (XEXP (op, 0)) == REG)
    {
      print_operand (file, XEXP (op, 0), letter);
    }
  else if (letter == 'm')
    fprintf (file, HOST_WIDE_INT_PRINT_DEC, (1L << INTVAL (op)));
  else
    output_addr_const (file, op);
}

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is ADDR.  ADDR is an RTL expression.

   Possible address classifications and output formats are,
   
   ADDRESS_REG                  "%0, r0"

   ADDRESS_REG with non-zero    "%0, <addr_const>"
   offset       

   ADDRESS_REG_INDEX            "rA, RB"    
                                (if rA is r0, rA and rB are swapped)

   ADDRESS_CONST_INT            "r0, <addr_const>"

   ADDRESS_SYMBOLIC             "rBase, <addr_const>"   
                                (rBase is a base register suitable for the 
				 symbol's type)
*/

void
print_operand_address (FILE * file, rtx addr)
{
  struct microblaze_address_info info;
  enum microblaze_address_type type;
  if (!microblaze_classify_address (&info, addr, GET_MODE (addr), 1))
    fatal_insn ("insn contains an invalid address !", addr);

  type = info.type;
  switch (info.type)
    {
    case ADDRESS_REG:
      fprintf (file, "%s,", reg_names[REGNO (info.regA)]);
      output_addr_const (file, info.offset);
      break;
    case ADDRESS_REG_INDEX:
      if (REGNO (info.regA) == 0)
	/* Make rB == r0 instead of rA == r0. This helps reduce read port 
           congestion.  */
	fprintf (file, "%s,%s", reg_names[REGNO (info.regB)],
		 reg_names[REGNO (info.regA)]);
      else if (REGNO (info.regB) != 0)
	/* This is a silly swap to help Dhrystone.  */
	fprintf (file, "%s,%s", reg_names[REGNO (info.regB)],
		 reg_names[REGNO (info.regA)]);
      break;
    case ADDRESS_CONST_INT:
      fprintf (file, "%s,", reg_names[REGNO (info.regA)]);
      output_addr_const (file, info.offset);
      break;
    case ADDRESS_SYMBOLIC:
    case ADDRESS_GOTOFF:
    case ADDRESS_PLT:
      if (info.regA)
	fprintf (file, "%s,", reg_names[REGNO (info.regA)]);
      output_addr_const (file, info.symbol);
      if (type == ADDRESS_GOTOFF)
	{
	  fputs ("@GOT", file);
	}
      else if (type == ADDRESS_PLT)
	{
	  fputs ("@PLT", file);
	}
      break;
    case ADDRESS_INVALID:
      fatal_insn ("invalid address", addr);
      break;
    }
}

/* Emit either a label, .comm, or .lcomm directive, and mark that the symbol
   is used, so that we don't emit an .extern for it in 
   microblaze_asm_file_end.  */

void
microblaze_declare_object (FILE * stream, const char *name,
			   const char *section, const char *fmt, int size)
{

  fputs (section, stream);	
  assemble_name (stream, name);
  fprintf (stream, fmt, size);
}

/* Common code to emit the insns (or to write the instructions to a file)
   to save/restore registers.

   Other parts of the code assume that MICROBLAZE_TEMP1_REGNUM (aka large_reg)
   is not modified within save_restore_insns.  */

#define BITSET_P(VALUE,BIT) (((VALUE) & (1L << (BIT))) != 0)

/* Save or restore instructions based on whether this is the prologue or 
   epilogue.  prologue is 1 for the prologue.  */
static void
save_restore_insns (int prologue)
{
  rtx base_reg_rtx, reg_rtx, mem_rtx, /* msr_rtx, */ isr_reg_rtx =
    0, isr_mem_rtx = 0;
  rtx isr_msr_rtx = 0, insn;
  long mask = current_frame_info.mask;
  HOST_WIDE_INT gp_offset;
  int regno;

  if (frame_pointer_needed
      && !BITSET_P (mask, HARD_FRAME_POINTER_REGNUM - GP_REG_FIRST))
    gcc_unreachable ();

  if (mask == 0)
    return;

  /* Save registers starting from high to low.  The debuggers prefer at least
     the return register be stored at func+4, and also it allows us not to
     need a nop in the epilog if at least one register is reloaded in
     addition to return address.  */

  /* Pick which pointer to use as a base register.  For small frames, just
     use the stack pointer.  Otherwise, use a temporary register.  Save 2
     cycles if the save area is near the end of a large frame, by reusing
     the constant created in the prologue/epilogue to adjust the stack
     frame.  */

  gp_offset = current_frame_info.gp_offset;

  gcc_assert (gp_offset > 0);

  base_reg_rtx = stack_pointer_rtx;

  /* For interrupt_handlers, need to save/restore the MSR.  */
  if (interrupt_handler)
    {
      isr_mem_rtx = gen_rtx_MEM (SImode,
				 gen_rtx_PLUS (Pmode, base_reg_rtx,
					       GEN_INT (current_frame_info.
							gp_offset -
							UNITS_PER_WORD)));

      /* Do not optimize in flow analysis.  */
      MEM_VOLATILE_P (isr_mem_rtx) = 1;
      isr_reg_rtx = gen_rtx_REG (SImode, MB_ABI_MSR_SAVE_REG);
      isr_msr_rtx = gen_rtx_REG (SImode, ST_REG);
    }

  if (interrupt_handler && !prologue)
    {
      emit_move_insn (isr_reg_rtx, isr_mem_rtx);
      emit_move_insn (isr_msr_rtx, isr_reg_rtx);
      /* Do not optimize in flow analysis.  */
      emit_insn (gen_rtx_USE (SImode, isr_reg_rtx));
      emit_insn (gen_rtx_USE (SImode, isr_msr_rtx));
    }

  for (regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    {
      if (BITSET_P (mask, regno - GP_REG_FIRST))
	{
	  if (regno == MB_ABI_SUB_RETURN_ADDR_REGNUM)
	    /* Don't handle here. Already handled as the first register.  */
	    continue;

	  reg_rtx = gen_rtx_REG (SImode, regno);
	  insn = gen_rtx_PLUS (Pmode, base_reg_rtx, GEN_INT (gp_offset));
	  mem_rtx = gen_rtx_MEM (SImode, insn);
	  if (interrupt_handler || save_volatiles)
	    /* Do not optimize in flow analysis.  */
	    MEM_VOLATILE_P (mem_rtx) = 1;

	  if (prologue)
	    {
	      insn = emit_move_insn (mem_rtx, reg_rtx);
	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	  else
	    {
	      insn = emit_move_insn (reg_rtx, mem_rtx);
	    }

	  gp_offset += GET_MODE_SIZE (SImode);
	}
    }

  if (interrupt_handler && prologue)
    {
      emit_move_insn (isr_reg_rtx, isr_msr_rtx);
      emit_move_insn (isr_mem_rtx, isr_reg_rtx);

      /* Do not optimize in flow analysis.  */
      emit_insn (gen_rtx_USE (SImode, isr_reg_rtx));
      emit_insn (gen_rtx_USE (SImode, isr_msr_rtx));
    }

  /* Done saving and restoring */
}


/* Set up the stack and frame (if desired) for the function.  */
static void
microblaze_function_prologue (FILE * file, HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  const char *fnname;
  long fsiz = current_frame_info.total_size;

  /* Get the function name the same way that toplev.c does before calling
     assemble_start_function.  This is needed so that the name used here
     exactly matches the name used in ASM_DECLARE_FUNCTION_NAME.  */
  fnname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
  if (!flag_inhibit_size_directive)
    {
      fputs ("\t.ent\t", file);
      if (interrupt_handler && strcmp (INTERRUPT_HANDLER_NAME, fnname))
	fputs ("_interrupt_handler", file);
      else
	assemble_name (file, fnname);
      fputs ("\n", file);
      if (!interrupt_handler)
	ASM_OUTPUT_TYPE_DIRECTIVE (file, fnname, "function");
    }

  assemble_name (file, fnname);
  fputs (":\n", file);

  if (interrupt_handler && strcmp (INTERRUPT_HANDLER_NAME, fnname))
    fputs ("_interrupt_handler:\n", file);

  if (!flag_inhibit_size_directive)
    {
      /* .frame FRAMEREG, FRAMESIZE, RETREG.  */
      fprintf (file,
	       "\t.frame\t%s,%ld,%s\t\t# vars= %ld, regs= %d, args= %d\n",
	       (reg_names[(frame_pointer_needed)
			  ? HARD_FRAME_POINTER_REGNUM :
			  STACK_POINTER_REGNUM]), fsiz,
	       reg_names[MB_ABI_SUB_RETURN_ADDR_REGNUM + GP_REG_FIRST],
	       current_frame_info.var_size, current_frame_info.num_gp,
	       crtl->outgoing_args_size);
      fprintf (file, "\t.mask\t0x%08lx\n", current_frame_info.mask);
    }
}

/* Output extra assembler code at the end of a prologue.  */
static void
microblaze_function_end_prologue (FILE * file)
{
  if (TARGET_STACK_CHECK)
    {
      fprintf (file, "\t# Stack Check Stub -- Start.\n\t");
      fprintf (file, "ori\tr18,r0,_stack_end\n\t");
      fprintf (file, "cmpu\tr18,r1,r18\n\t");
      fprintf (file, "bgei\tr18,_stack_overflow_exit\n\t");
      fprintf (file, "# Stack Check Stub -- End.\n");
    }
}

/* Expand the prologue into a bunch of separate insns.  */

void
microblaze_expand_prologue (void)
{
  int regno;
  HOST_WIDE_INT fsiz;
  const char *arg_name = 0;
  tree fndecl = current_function_decl;
  tree fntype = TREE_TYPE (fndecl);
  tree fnargs = DECL_ARGUMENTS (fndecl);
  rtx next_arg_reg;
  int i;
  tree next_arg;
  tree cur_arg;
  CUMULATIVE_ARGS args_so_far_v;
  cumulative_args_t args_so_far;
  rtx mem_rtx, reg_rtx;

  /* If struct value address is treated as the first argument, make it so.  */
  if (aggregate_value_p (DECL_RESULT (fndecl), fntype)
      && !cfun->returns_pcc_struct)
    {
      tree type = build_pointer_type (fntype);
      tree function_result_decl = build_decl (BUILTINS_LOCATION, PARM_DECL, 
					      NULL_TREE, type);

      DECL_ARG_TYPE (function_result_decl) = type;
      TREE_CHAIN (function_result_decl) = fnargs;
      fnargs = function_result_decl;
    }

  /* Determine the last argument, and get its name.  */

  INIT_CUMULATIVE_ARGS (args_so_far_v, fntype, NULL_RTX, 0, 0);
  args_so_far = pack_cumulative_args (&args_so_far_v);
  regno = GP_ARG_FIRST;

  for (cur_arg = fnargs; cur_arg != 0; cur_arg = next_arg)
    {
      tree passed_type = DECL_ARG_TYPE (cur_arg);
      enum machine_mode passed_mode = TYPE_MODE (passed_type);
      rtx entry_parm;

      if (TREE_ADDRESSABLE (passed_type))
	{
	  passed_type = build_pointer_type (passed_type);
	  passed_mode = Pmode;
	}

      entry_parm = targetm.calls.function_arg (args_so_far, passed_mode,
					       passed_type, true);

      if (entry_parm)
	{
	  int words;

	  /* passed in a register, so will get homed automatically.  */
	  if (GET_MODE (entry_parm) == BLKmode)
	    words = (int_size_in_bytes (passed_type) + 3) / 4;
	  else
	    words = (GET_MODE_SIZE (GET_MODE (entry_parm)) + 3) / 4;

	  regno = REGNO (entry_parm) + words - 1;
	}
      else
	{
	  regno = GP_ARG_LAST + 1;
	  break;
	}

      targetm.calls.function_arg_advance (args_so_far, passed_mode,
					  passed_type, true);

      next_arg = TREE_CHAIN (cur_arg);
      if (next_arg == 0)
	{
	  if (DECL_NAME (cur_arg))
	    arg_name = IDENTIFIER_POINTER (DECL_NAME (cur_arg));

	  break;
	}
    }

  /* Split parallel insn into a sequence of insns.  */

  next_arg_reg = targetm.calls.function_arg (args_so_far, VOIDmode,
					     void_type_node, true);
  if (next_arg_reg != 0 && GET_CODE (next_arg_reg) == PARALLEL)
    {
      rtvec adjust = XVEC (next_arg_reg, 0);
      int num = GET_NUM_ELEM (adjust);

      for (i = 0; i < num; i++)
	{
	  rtx pattern = RTVEC_ELT (adjust, i);
	  emit_insn (pattern);
	}
    }

  fsiz = compute_frame_size (get_frame_size ());

  /* If this function is a varargs function, store any registers that
     would normally hold arguments ($5 - $10) on the stack.  */
  if (((TYPE_ARG_TYPES (fntype) != 0
	&& (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
	    != void_type_node))
       || (arg_name != 0
	   && ((arg_name[0] == '_'
		&& strcmp (arg_name, "__builtin_va_alist") == 0)
	       || (arg_name[0] == 'v'
		   && strcmp (arg_name, "va_alist") == 0)))))
    {
      int offset = (regno - GP_ARG_FIRST + 1) * UNITS_PER_WORD;
      rtx ptr = stack_pointer_rtx;

      /* If we are doing svr4-abi, sp has already been decremented by fsiz. */
      for (; regno <= GP_ARG_LAST; regno++)
	{
	  if (offset != 0)
	    ptr = gen_rtx_PLUS (Pmode, stack_pointer_rtx, GEN_INT (offset));
	  emit_move_insn (gen_rtx_MEM (SImode, ptr),
			  gen_rtx_REG (SImode, regno));

	  offset += GET_MODE_SIZE (SImode);
	}

    }

  if (fsiz > 0)
    {
      rtx fsiz_rtx = GEN_INT (fsiz);

      rtx insn = NULL;
      insn = emit_insn (gen_subsi3 (stack_pointer_rtx, stack_pointer_rtx,
				    fsiz_rtx));
      if (insn)
	RTX_FRAME_RELATED_P (insn) = 1;

      /* Handle SUB_RETURN_ADDR_REGNUM specially at first.  */
      if (!crtl->is_leaf || interrupt_handler)
	{
	  mem_rtx = gen_rtx_MEM (SImode,
				 gen_rtx_PLUS (Pmode, stack_pointer_rtx,
					       const0_rtx));

	  if (interrupt_handler)
	    /* Do not optimize in flow analysis.  */
	    MEM_VOLATILE_P (mem_rtx) = 1;

	  reg_rtx = gen_rtx_REG (SImode, MB_ABI_SUB_RETURN_ADDR_REGNUM);
	  insn = emit_move_insn (mem_rtx, reg_rtx);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      /* _save_ registers for prologue.  */
      save_restore_insns (1);

      if (frame_pointer_needed)
	{
	  rtx insn = 0;

	  insn = emit_insn (gen_movsi (hard_frame_pointer_rtx,
				       stack_pointer_rtx));

	  if (insn)
	    RTX_FRAME_RELATED_P (insn) = 1;
	}
    }

  if (flag_pic == 2 && df_regs_ever_live_p (MB_ABI_PIC_ADDR_REGNUM))
    {
      SET_REGNO (pic_offset_table_rtx, MB_ABI_PIC_ADDR_REGNUM);
      emit_insn (gen_set_got (pic_offset_table_rtx));	/* setting GOT.  */
    }

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  */

  if (profile_flag)
    emit_insn (gen_blockage ());
}

/* Do necessary cleanup after a function to restore stack, frame, and regs.  */

#define RA_MASK ((long) 0x80000000)	/* 1 << 31 */
#define PIC_OFFSET_TABLE_MASK (1 << (PIC_OFFSET_TABLE_REGNUM - GP_REG_FIRST))

static void
microblaze_function_epilogue (FILE * file ATTRIBUTE_UNUSED,
			      HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  const char *fnname;

  /* Get the function name the same way that toplev.c does before calling
     assemble_start_function.  This is needed so that the name used here
     exactly matches the name used in ASM_DECLARE_FUNCTION_NAME.  */
  fnname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);

  if (!flag_inhibit_size_directive)
    {
      fputs ("\t.end\t", file);
      if (interrupt_handler)
	fputs ("_interrupt_handler", file);
      else
	assemble_name (file, fnname);
      fputs ("\n", file);
    }

  /* Reset state info for each function.  */
  current_frame_info = zero_frame_info;

  /* Restore the output file if optimizing the GP (optimizing the GP causes
     the text to be diverted to a tempfile, so that data decls come before
     references to the data).  */
}

/* Expand the epilogue into a bunch of separate insns.  */

void
microblaze_expand_epilogue (void)
{
  HOST_WIDE_INT fsiz = current_frame_info.total_size;
  rtx fsiz_rtx = GEN_INT (fsiz);
  rtx reg_rtx;
  rtx mem_rtx;

  /* In case of interrupt handlers use addki instead of addi for changing the 
     stack pointer value.  */

  if (microblaze_can_use_return_insn ())
    {
      emit_jump_insn (gen_return_internal (gen_rtx_REG (Pmode,
							GP_REG_FIRST +
							MB_ABI_SUB_RETURN_ADDR_REGNUM)));
      return;
    }

  if (fsiz > 0)
    {
      /* Restore SUB_RETURN_ADDR_REGNUM at first. This is to prevent the 
         sequence of load-followed by a use (in rtsd) in every prologue. Saves 
         a load-use stall cycle  :)   This is also important to handle alloca. 
         (See comments for if (frame_pointer_needed) below.  */

      if (!crtl->is_leaf || interrupt_handler)
	{
	  mem_rtx =
	    gen_rtx_MEM (SImode,
			 gen_rtx_PLUS (Pmode, stack_pointer_rtx, const0_rtx));
	  if (interrupt_handler)
	    /* Do not optimize in flow analysis.  */
	    MEM_VOLATILE_P (mem_rtx) = 1;
	  reg_rtx = gen_rtx_REG (SImode, MB_ABI_SUB_RETURN_ADDR_REGNUM);
	  emit_move_insn (reg_rtx, mem_rtx);
	}

      /* It is important that this is done after we restore the return address 
         register (above).  When alloca is used, we want to restore the 
	 sub-routine return address only from the current stack top and not 
	 from the frame pointer (which we restore below). (frame_pointer + 0) 
	 might have been over-written since alloca allocates memory on the 
	 current stack.  */
      if (frame_pointer_needed)
	emit_insn (gen_movsi (stack_pointer_rtx, hard_frame_pointer_rtx));

      /* _restore_ registers for epilogue.  */
      save_restore_insns (0);
      emit_insn (gen_blockage ());
      emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, fsiz_rtx));
    }

  emit_jump_insn (gen_return_internal (gen_rtx_REG (Pmode, GP_REG_FIRST +
						    MB_ABI_SUB_RETURN_ADDR_REGNUM)));
}


/* Return nonzero if this function is known to have a null epilogue.
   This allows the optimizer to omit jumps to jumps if no stack
   was created.  */

int
microblaze_can_use_return_insn (void)
{
  if (!reload_completed)
    return 0;

  if (df_regs_ever_live_p (MB_ABI_SUB_RETURN_ADDR_REGNUM) || profile_flag)
    return 0;

  if (current_frame_info.initialized)
    return current_frame_info.total_size == 0;

  return compute_frame_size (get_frame_size ()) == 0;
}

/* Implement TARGET_SECONDARY_RELOAD.  */

static reg_class_t
microblaze_secondary_reload (bool in_p ATTRIBUTE_UNUSED, rtx x ATTRIBUTE_UNUSED, 
			     reg_class_t rclass, enum machine_mode mode ATTRIBUTE_UNUSED, 
			     secondary_reload_info *sri ATTRIBUTE_UNUSED)
{
  if (rclass == ST_REGS)
    return GR_REGS;

  return NO_REGS;
}

static void
microblaze_globalize_label (FILE * stream, const char *name)
{
  fputs ("\t.globl\t", stream);
  if (interrupt_handler && strcmp (name, INTERRUPT_HANDLER_NAME))
    {
      fputs (INTERRUPT_HANDLER_NAME, stream);
      fputs ("\n\t.globl\t", stream);
    }
  assemble_name (stream, name);
  fputs ("\n", stream);
}

/* Returns true if decl should be placed into a "small data" section.  */
static bool
microblaze_elf_in_small_data_p (const_tree decl)
{
  HOST_WIDE_INT size;

  if (!TARGET_XLGPOPT)
    return false;

  /* We want to merge strings, so we never consider them small data.  */
  if (TREE_CODE (decl) == STRING_CST)
    return false;

  /* Functions are never in the small data area.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    return false;

  if (TREE_CODE (decl) == VAR_DECL && DECL_SECTION_NAME (decl))
    {
      const char *section = TREE_STRING_POINTER (DECL_SECTION_NAME (decl));
      if (strcmp (section, ".sdata") == 0
	  || strcmp (section, ".sdata2") == 0
	  || strcmp (section, ".sbss") == 0
	  || strcmp (section, ".sbss2") == 0)
	return true;
    }

  size = int_size_in_bytes (TREE_TYPE (decl));

  return (size > 0 && size <= microblaze_section_threshold);
}


static section *
microblaze_select_section (tree decl, int reloc, unsigned HOST_WIDE_INT align)
{
  switch (categorize_decl_for_section (decl, reloc))
    {
    case SECCAT_RODATA_MERGE_STR:
    case SECCAT_RODATA_MERGE_STR_INIT:
      /* MB binutils have various issues with mergeable string sections and
         relaxation/relocation. Currently, turning mergeable sections 
         into regular readonly sections.  */

      return readonly_data_section;
    default:
      return default_elf_select_section (decl, reloc, align);
    }
}

/*
  Encode info about sections into the RTL based on a symbol's declaration.
  The default definition of this hook, default_encode_section_info in 
  `varasm.c', sets a number of commonly-useful bits in SYMBOL_REF_FLAGS. */

static void
microblaze_encode_section_info (tree decl, rtx rtl, int first)
{
  default_encode_section_info (decl, rtl, first);
}

static rtx
expand_pic_symbol_ref (enum machine_mode mode ATTRIBUTE_UNUSED, rtx op)
{
  rtx result;
  result = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, op), UNSPEC_GOTOFF);
  result = gen_rtx_CONST (Pmode, result);
  result = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, result);
  result = gen_const_mem (Pmode, result);
  return result;
}

bool
microblaze_expand_move (enum machine_mode mode, rtx operands[])
{
  /* If operands[1] is a constant address invalid for pic, then we need to
     handle it just like LEGITIMIZE_ADDRESS does.  */
  if (flag_pic)
    {
      if (GET_CODE (operands[0]) == MEM)
	{
	  rtx addr = XEXP (operands[0], 0);
	  if (GET_CODE (addr) == SYMBOL_REF)
	    {
	      rtx ptr_reg, result;

	      if (reload_in_progress)
		df_set_regs_ever_live (PIC_OFFSET_TABLE_REGNUM, true);

	      addr = expand_pic_symbol_ref (mode, addr);
	      ptr_reg = gen_reg_rtx (Pmode);
	      emit_move_insn (ptr_reg, addr);
	      result = gen_rtx_MEM (mode, ptr_reg);
	      operands[0] = result;
	    }
	}
      if (GET_CODE (operands[1]) == SYMBOL_REF
	  || GET_CODE (operands[1]) == LABEL_REF)
	{
	  rtx result;
	  if (reload_in_progress)
	    df_set_regs_ever_live (PIC_OFFSET_TABLE_REGNUM, true);
	  result = expand_pic_symbol_ref (mode, operands[1]);
	  if (GET_CODE (operands[0]) != REG)
	    {
	      rtx ptr_reg = gen_reg_rtx (Pmode);
	      emit_move_insn (ptr_reg, result);
	      emit_move_insn (operands[0], ptr_reg);
	    }
	  else
	    {
	      emit_move_insn (operands[0], result);
	    }
	  return true;
	}
      else if (GET_CODE (operands[1]) == MEM &&
	       GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF)
	{
	  rtx result;
	  rtx ptr_reg;
	  if (reload_in_progress)
	    df_set_regs_ever_live (PIC_OFFSET_TABLE_REGNUM, true);
	  result = expand_pic_symbol_ref (mode, XEXP (operands[1], 0));

	  ptr_reg = gen_reg_rtx (Pmode);

	  emit_move_insn (ptr_reg, result);
	  result = gen_rtx_MEM (mode, ptr_reg);
	  emit_move_insn (operands[0], result);
	  return true;
	}
      else if (pic_address_needs_scratch (operands[1]))
	{
	  rtx temp = force_reg (SImode, XEXP (XEXP (operands[1], 0), 0));
	  rtx temp2 = XEXP (XEXP (operands[1], 0), 1);

	  if (reload_in_progress)
	    df_set_regs_ever_live (PIC_OFFSET_TABLE_REGNUM, true);
	  emit_move_insn (operands[0], gen_rtx_PLUS (SImode, temp, temp2));
	  return true;
	}
    }

  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (operands[0], SImode)
      && !register_operand (operands[1], SImode)
      && (GET_CODE (operands[1]) != CONST_INT || INTVAL (operands[1]) != 0))
    {
      rtx temp = force_reg (SImode, operands[1]);
      emit_move_insn (operands[0], temp);
      return true;
    }
  return false;
}

/* Expand shift operations.  */
int
microblaze_expand_shift (rtx operands[])
{
  gcc_assert ((GET_CODE (operands[2]) == CONST_INT)
	      || (GET_CODE (operands[2]) == REG)
	      || (GET_CODE (operands[2]) == SUBREG));

  /* Shift by one -- generate pattern.  */
  if ((GET_CODE (operands[2]) == CONST_INT) && (INTVAL (operands[2]) == 1))
    return 0;

  /* Have barrel shifter and shift > 1: use it.  */
  if (TARGET_BARREL_SHIFT)
    return 0;

  gcc_assert ((GET_CODE (operands[0]) == REG)
	      || (GET_CODE (operands[0]) == SUBREG)
	      || (GET_CODE (operands[1]) == REG)
	      || (GET_CODE (operands[1]) == SUBREG));

  /* Shift by zero -- copy regs if necessary.  */
  if ((GET_CODE (operands[2]) == CONST_INT) && (INTVAL (operands[2]) == 0))
    {
      if (REGNO (operands[0]) != REGNO (operands[1]))
	emit_insn (gen_movsi (operands[0], operands[1]));
      return 1;
    }

  return 0;
}

/* Return an RTX indicating where the return address to the
   calling function can be found.  */
rtx
microblaze_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return NULL_RTX;

  return gen_rtx_PLUS (Pmode,
		       get_hard_reg_initial_val (Pmode,
						 MB_ABI_SUB_RETURN_ADDR_REGNUM),
		       GEN_INT (8));
}

/* Queue an .ident string in the queue of top-level asm statements.
   If the string size is below the threshold, put it into .sdata2.
   If the front-end is done, we must be being called from toplev.c.
   In that case, do nothing.  */
void 
microblaze_asm_output_ident (const char *string)
{
  const char *section_asm_op;
  int size;
  char *buf;

  if (cgraph_state != CGRAPH_STATE_PARSING)
    return;

  size = strlen (string) + 1;
  if (size <= microblaze_section_threshold)
    section_asm_op = SDATA2_SECTION_ASM_OP;
  else
    section_asm_op = READONLY_DATA_SECTION_ASM_OP;

  buf = ACONCAT ((section_asm_op, "\n\t.ascii \"", string, "\\0\"\n", NULL));
  add_asm_node (build_string (strlen (buf), buf));
}

static void
microblaze_elf_asm_init_sections (void)
{
  sdata2_section
    = get_unnamed_section (SECTION_WRITE, output_section_asm_op,
			   SDATA2_SECTION_ASM_OP);
}

/*  Generate assembler code for constant parts of a trampoline.  */

static void
microblaze_asm_trampoline_template (FILE *f)
{
  fprintf (f, "\t.word\t0x03e00821\t\t# move   $1,$31\n");
  fprintf (f, "\t.word\t0x04110001\t\t# bgezal $0,.+8\n");
  fprintf (f, "\t.word\t0x00000000\t\t# nop\n");
  fprintf (f, "\t.word\t0x8fe30014\t\t# lw     $3,20($31)\n");
  fprintf (f, "\t.word\t0x8fe20018\t\t# lw     $2,24($31)\n");
  fprintf (f, "\t.word\t0x0060c821\t\t# move   $25,$3 (abicalls)\n");
  fprintf (f, "\t.word\t0x00600008\t\t# jr     $3\n");
  fprintf (f, "\t.word\t0x0020f821\t\t# move   $31,$1\n");
  /* fprintf (f, "\t.word\t0x00000000\t\t# <function address>\n");  */
  /* fprintf (f, "\t.word\t0x00000000\t\t# <static chain value>\n");  */
}

/* Implement TARGET_TRAMPOLINE_INIT.  */

static void
microblaze_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx mem;

  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (8*UNITS_PER_WORD), BLOCK_OP_NORMAL);

  mem = adjust_address (m_tramp, SImode, 8);
  emit_move_insn (mem, chain_value);
  mem = adjust_address (m_tramp, SImode, 12);
  emit_move_insn (mem, fnaddr);
}

/* Emit instruction to perform compare.  
   cmp is (compare_op op0 op1).  */
static rtx
microblaze_emit_compare (enum machine_mode mode, rtx cmp, enum rtx_code *cmp_code)
{
  rtx cmp_op0 = XEXP (cmp, 0);
  rtx cmp_op1 = XEXP (cmp, 1);
  rtx comp_reg = gen_reg_rtx (SImode);
  enum rtx_code code = *cmp_code;
  
  gcc_assert ((GET_CODE (cmp_op0) == REG) || (GET_CODE (cmp_op0) == SUBREG));

  /* If comparing against zero, just test source reg.  */
  if (cmp_op1 == const0_rtx) 
    return cmp_op0;

  if (code == EQ || code == NE)
    {
      if (TARGET_PATTERN_COMPARE && GET_CODE(cmp_op1) == REG) 
        {
          if (code == EQ) 
	    {
	      emit_insn (gen_seq_internal_pat (comp_reg, cmp_op0, cmp_op1));
	      *cmp_code = NE;
	    }
	  else
	    {    
	      emit_insn (gen_sne_internal_pat (comp_reg, cmp_op0, cmp_op1));
	    }
        }
      else
	/* Use xor for equal/not-equal comparison.  */
	emit_insn (gen_xorsi3 (comp_reg, cmp_op0, cmp_op1));
    }
  else if (code == GT || code == GTU || code == LE || code == LEU)
    {
      /* MicroBlaze compare is not symmetrical.  */
      /* Swap argument order.  */
      cmp_op1 = force_reg (mode, cmp_op1);
      if (code == GT || code == LE) 
        emit_insn (gen_signed_compare (comp_reg, cmp_op0, cmp_op1));
      else
        emit_insn (gen_unsigned_compare (comp_reg, cmp_op0, cmp_op1));
      /* Translate test condition.  */
      *cmp_code = swap_condition (code);
    }
  else /* if (code == GE || code == GEU || code == LT || code == LTU) */
    {
      cmp_op1 = force_reg (mode, cmp_op1);
      if (code == GE || code == LT) 
        emit_insn (gen_signed_compare (comp_reg, cmp_op1, cmp_op0));
      else
        emit_insn (gen_unsigned_compare (comp_reg, cmp_op1, cmp_op0));
    }

  return comp_reg;
}

/* Generate conditional branch -- first, generate test condition,
   second, generate correct branch instruction.  */

void
microblaze_expand_conditional_branch (enum machine_mode mode, rtx operands[])
{
  enum rtx_code code = GET_CODE (operands[0]);
  rtx comp;
  rtx condition;

  comp = microblaze_emit_compare (mode, operands[0], &code);
  condition = gen_rtx_fmt_ee (signed_condition (code), SImode, comp, const0_rtx);
  emit_jump_insn (gen_condjump (condition, operands[3]));
}

void
microblaze_expand_conditional_branch_sf (rtx operands[])
{
  rtx condition;
  rtx cmp_op0 = XEXP (operands[0], 0);
  rtx cmp_op1 = XEXP (operands[0], 1);
  rtx comp_reg = gen_reg_rtx (SImode);

  emit_insn (gen_cstoresf4 (comp_reg, operands[0], cmp_op0, cmp_op1));
  condition = gen_rtx_NE (SImode, comp_reg, const0_rtx);
  emit_jump_insn (gen_condjump (condition, operands[3]));
}

/* Implement TARGET_FRAME_POINTER_REQUIRED.  */

static bool
microblaze_frame_pointer_required (void)
{
  /* If the function contains dynamic stack allocations, we need to
     use the frame pointer to access the static parts of the frame.  */
  if (cfun->calls_alloca)
    return true;
  return false;
}

void
microblaze_expand_divide (rtx operands[])
{
  /* Table lookup software divides. Works for all (nr/dr) where (0 <= nr,dr <= 15).  */

  rtx regt1 = gen_reg_rtx (SImode); 
  rtx reg18 = gen_rtx_REG (SImode, R_TMP);
  rtx regqi = gen_reg_rtx (QImode);
  rtx div_label = gen_label_rtx ();
  rtx div_end_label = gen_label_rtx ();
  rtx div_table_rtx = gen_rtx_SYMBOL_REF (QImode,"_divsi3_table");
  rtx mem_rtx;
  rtx ret;
  rtx jump, cjump, insn;

  insn = emit_insn (gen_iorsi3 (regt1, operands[1], operands[2]));
  cjump = emit_jump_insn_after (gen_cbranchsi4 (
					gen_rtx_GTU (SImode, regt1, GEN_INT (15)), 
					regt1, GEN_INT (15), div_label), insn);
  LABEL_NUSES (div_label) = 1; 
  JUMP_LABEL (cjump) = div_label;
  emit_insn (gen_rtx_CLOBBER (SImode, reg18));

  emit_insn (gen_ashlsi3_bshift (regt1, operands[1], GEN_INT(4)));
  emit_insn (gen_addsi3 (regt1, regt1, operands[2]));
  mem_rtx = gen_rtx_MEM (QImode,
                            gen_rtx_PLUS (Pmode, regt1, div_table_rtx));

  insn = emit_insn (gen_movqi (regqi, mem_rtx)); 
  insn = emit_insn (gen_movsi (operands[0], gen_rtx_SUBREG (SImode, regqi, 0)));
  jump = emit_jump_insn_after (gen_jump (div_end_label), insn); 
  JUMP_LABEL (jump) = div_end_label;
  LABEL_NUSES (div_end_label) = 1; 
  emit_barrier ();

  emit_label (div_label);
  ret = emit_library_call_value (gen_rtx_SYMBOL_REF (Pmode, "__divsi3"), 
				       operands[0], LCT_NORMAL, 
				       GET_MODE (operands[0]), 2, operands[1], 
				       GET_MODE (operands[1]), operands[2], 
				       GET_MODE (operands[2]));
  if (ret != operands[0])
                emit_move_insn (operands[0], ret);    

  emit_label (div_end_label);
  emit_insn (gen_blockage ());
}

/* Implement TARGET_FUNCTION_VALUE.  */
static rtx
microblaze_function_value (const_tree valtype,
			   const_tree func ATTRIBUTE_UNUSED,
			   bool outgoing ATTRIBUTE_UNUSED)
{
  return LIBCALL_VALUE (TYPE_MODE (valtype));
}

/* Implement TARGET_SCHED_ADJUST_COST.  */
static int
microblaze_adjust_cost (rtx insn ATTRIBUTE_UNUSED, rtx link,
			rtx dep ATTRIBUTE_UNUSED, int cost)
{
  if (REG_NOTE_KIND (link) == REG_DEP_OUTPUT)
    return cost;
  if (REG_NOTE_KIND (link) != 0)
    return 0;
  return cost;
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P.

   At present, GAS doesn't understand li.[sd], so don't allow it
   to be generated at present.  */
static bool
microblaze_legitimate_constant_p (enum machine_mode mode, rtx x)
{
  return GET_CODE (x) != CONST_DOUBLE || microblaze_const_double_ok (x, mode);
}

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO      microblaze_encode_section_info

#undef TARGET_ASM_GLOBALIZE_LABEL
#define TARGET_ASM_GLOBALIZE_LABEL      microblaze_globalize_label

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE    microblaze_function_prologue

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE    microblaze_function_epilogue

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS                microblaze_rtx_costs

#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST             microblaze_address_cost

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE          microblaze_attribute_table

#undef TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P          microblaze_elf_in_small_data_p

#undef TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION       microblaze_select_section

#undef TARGET_HAVE_SRODATA_SECTION
#define TARGET_HAVE_SRODATA_SECTION     true

#undef TARGET_ASM_FUNCTION_END_PROLOGUE
#define TARGET_ASM_FUNCTION_END_PROLOGUE \
                                        microblaze_function_end_prologue

#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES	function_arg_partial_bytes

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG		microblaze_function_arg

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE	microblaze_function_arg_advance

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE 		microblaze_can_eliminate

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS 	microblaze_legitimize_address

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P 	microblaze_legitimate_address_p 

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED	microblaze_frame_pointer_required

#undef  TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE	microblaze_asm_trampoline_template

#undef  TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT		microblaze_trampoline_init

#undef  TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE 	default_promote_function_mode_always_promote

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE		microblaze_function_value 

#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD		microblaze_secondary_reload

#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST	microblaze_adjust_cost

#undef TARGET_ASM_INIT_SECTIONS
#define TARGET_ASM_INIT_SECTIONS	microblaze_elf_asm_init_sections

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE		microblaze_option_override 

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P microblaze_legitimate_constant_p

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-microblaze.h"
