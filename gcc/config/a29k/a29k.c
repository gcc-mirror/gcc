/* Subroutines used for code generation on AMD Am29000.
   Copyright (C) 1987, 88, 90-95, 97-99, 2000 Free Software
   Foundation, Inc. 
   Contributed by Richard Kenner (kenner@nyu.edu)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "function.h"
#include "expr.h"
#include "obstack.h"
#include "tree.h"
#include "reload.h"
#include "tm_p.h"

static int shift_constant_operand PARAMS ((rtx, enum machine_mode, int));
static void a29k_set_memflags_1 PARAMS ((rtx, int, int, int, int));
static void compute_regstack_size PARAMS ((void));
static void check_epilogue_internal_label PARAMS ((FILE *));

#define min(A,B)	((A) < (B) ? (A) : (B))

/* This gives the size in words of the register stack for the current
   procedure.  */

static int a29k_regstack_size;

/* True if the current procedure has a call instruction.  */

static int a29k_makes_calls;

/* This points to the last insn of the insn prologue.  It is set when
   an insn without a filled delay slot is found near the start of the
   function.  */

static char *a29k_last_prologue_insn;

/* This points to the first insn that will be in the epilogue.  It is null if
   no epilogue is required.  */

static char *a29k_first_epilogue_insn;

/* This is nonzero if a a29k_first_epilogue_insn was put in a delay slot.  It
   indicates that an intermediate label needs to be written.  */

static int a29k_first_epilogue_insn_used;

/* Location to hold the name of the current function.  We need this prolog to
   contain the tag words prior to the declaration.  So the name must be stored
   away.  */

const char *a29k_function_name;

/* Mapping of registers to debug register numbers.  The only change is
   for the frame pointer and the register numbers used for the incoming
   arguments.  */

int a29k_debug_reg_map[FIRST_PSEUDO_REGISTER];

/* Save information from a "cmpxx" operation until the branch or scc is
   emitted.  */

rtx a29k_compare_op0, a29k_compare_op1;
int a29k_compare_fp_p;

/* Returns 1 if OP is a 8-bit constant. */

int
cint_8_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return GET_CODE (op) == CONST_INT && (INTVAL (op) & 0xffffff00) == 0;
}

/* Returns 1 if OP is a 16-bit constant.  */

int
cint_16_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return GET_CODE (op) == CONST_INT && (INTVAL (op) & 0xffff0000) == 0;
}

/* Returns 1 if OP is a constant that cannot be moved in a single insn.  */

int
long_const_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (! CONSTANT_P (op))
    return 0;

  if (TARGET_29050 && GET_CODE (op) == CONST_INT
      && (INTVAL (op) & 0xffff) == 0)
    return 0;

  return (GET_CODE (op) != CONST_INT
	  || ((INTVAL (op) & 0xffff0000) != 0
	      && (INTVAL (op) & 0xffff0000) != 0xffff0000
	      && INTVAL (op) != 0x80000000));
}

/* The following four functions detect constants of 0, 8, 16, and 24 used as
   a position in ZERO_EXTRACT operations.  They can either be the appropriate
   constant integer or a shift (which will be produced by combine).  */

static int
shift_constant_operand (op, mode, val)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     int val;
{
  return ((GET_CODE (op) == CONST_INT && INTVAL (op) == val)
	  || (GET_CODE (op) == ASHIFT
	      && GET_CODE (XEXP (op, 0)) == CONST_INT
	      && INTVAL (XEXP (op, 0)) == val / 8
	      && GET_CODE (XEXP (op, 1)) == CONST_INT
	      && INTVAL (XEXP (op, 1)) == 3));
}

int
const_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return shift_constant_operand (op, mode, 0);
}

int
const_8_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return shift_constant_operand (op, mode, 8);
}

int
const_16_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return shift_constant_operand (op, mode, 16);
}

int
const_24_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return shift_constant_operand (op, mode, 24);
}

/* Returns 1 if OP is a floating-point constant of the proper mode.  */

int
float_const_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return GET_CODE (op) == CONST_DOUBLE && GET_MODE (op) == mode;
}

/* Returns 1 if OP is a floating-point constant of the proper mode or a
   general-purpose register.  */

int
gpc_reg_or_float_constant_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return float_const_operand (op, mode) || gpc_reg_operand (op, mode);
}

/* Returns 1 if OP is an integer constant of the proper mode or a
   general-purpose register.  */

int
gpc_reg_or_integer_constant_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_MODE (op) == VOIDmode
	   && (GET_CODE (op) == CONST_INT || GET_CODE (op) == CONST_DOUBLE))
	  || gpc_reg_operand (op, mode));
}
     
/* Returns 1 if OP is a special machine register.  */

int
spec_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != REG || GET_MODE (op) != mode)
    return 0;

  switch (GET_MODE_CLASS (mode))
    {
    case MODE_PARTIAL_INT:
      return REGNO (op) >= R_BP && REGNO (op) <= R_CR;
    case MODE_INT:
      return REGNO (op) >= R_Q && REGNO (op) <= R_EXO;
    default:
      return 0;
    }
}

/* Returns 1 if OP is an accumulator register.  */

int
accum_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == REG
	  && REGNO (op) >= R_ACU (0) && REGNO (op) <= R_ACU (3));
}

/* Returns 1 if OP is a normal data register.  */

int
gpc_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_CODE (op) == REG)
    regno = REGNO (op);
  else if (GET_CODE (op) == SUBREG && GET_CODE (SUBREG_REG (op)) == REG)
    {
      regno = REGNO (SUBREG_REG (op));
      if (regno < FIRST_PSEUDO_REGISTER)
	regno += SUBREG_WORD (op);
    }
  else
    return 0;

  return (regno >= FIRST_PSEUDO_REGISTER || regno < R_BP
	  || (regno >= R_KR (0) && regno <= R_KR (31)));
}

/* Returns 1 if OP is either an 8-bit constant integer or a general register.
   If a register, it must be in the proper mode unless MODE is VOIDmode.  */

int
srcb_operand (op, mode)
      register rtx op;
      enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT
      && (mode == QImode
	  || (INTVAL (op) & 0xffffff00) == 0))
    return 1;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  return gpc_reg_operand (op, mode);
}

int
cmplsrcb_operand (op, mode)
      register rtx op;
      enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT
      && (mode == QImode
	  || (INTVAL (op) & 0xffffff00) == 0xffffff00))
    return 1;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  return gpc_reg_operand (op, mode);
}

/* Return 1 if OP is either an immediate or a general register.  This is used
   for the input operand of mtsr/mtrsim.  */

int
gpc_reg_or_immediate_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return gpc_reg_operand (op, mode) || immediate_operand (op, mode);
}

/* Return 1 if OP can be used as the second operand of and AND insn.  This
   includes srcb_operand and a constant whose complement fits in 8 bits.  */

int
and_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (srcb_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && ((unsigned) ((~ INTVAL (op)) & GET_MODE_MASK (mode)) < 256)));
}

/* Return 1 if OP can be used as the second operand of an ADD insn.
   This is the same as above, except we use negative, rather than
   complement.   */

int
add_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (srcb_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && ((unsigned) ((- INTVAL (op)) & GET_MODE_MASK (mode)) < 256)));
}

/* Return 1 if OP is a valid address in a CALL_INSN.  These are a SYMBOL_REF
   to the current function, all SYMBOL_REFs if TARGET_SMALL_MEMORY, or
   a sufficiently-small constant.  */

int
call_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
      return (TARGET_SMALL_MEMORY
	      || (! TARGET_LARGE_MEMORY
		  && ((GET_CODE (op) == SYMBOL_REF && SYMBOL_REF_FLAG (op))
		      || ! strcmp (XSTR (op, 0), current_function_name))));

    case CONST_INT:
      return (unsigned HOST_WIDE_INT) INTVAL (op) < 0x40000;

    default:
      return 0;
    }
}

/* Return 1 if OP can be used as the input operand for a move insn.  */

int
in_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! general_operand (op, mode))
    return 0;

  while (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  switch (GET_CODE (op))
    {
    case REG:
      return 1;

    case MEM:
      return (GET_MODE_SIZE (mode) >= UNITS_PER_WORD || TARGET_DW_ENABLE);

    case CONST_INT:
      if (GET_MODE_CLASS (mode) != MODE_INT
	  && GET_MODE_CLASS (mode) != MODE_PARTIAL_INT)
	return 0;

      return 1;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return (GET_MODE (op) == mode
	      || mode == SImode || mode == HImode || mode == QImode);

    case CONST_DOUBLE:
      return ((GET_MODE_CLASS (mode) == MODE_FLOAT
	       && mode == GET_MODE (op))
	      || (GET_MODE (op) == VOIDmode
		  && GET_MODE_CLASS (mode) == MODE_INT));

    default:
      return 0;
    }
}

/* Return 1 if OP can be used as the output operand for a move insn.  */

int
out_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  rtx orig_op = op;

  if (! general_operand (op, mode))
    return 0;

  while (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (GET_CODE (op) == REG)
    return (gpc_reg_operand (orig_op, mode)
	    || spec_reg_operand (orig_op, mode)
	    || (GET_MODE_CLASS (mode) == MODE_FLOAT
		&& accum_reg_operand (orig_op, mode)));

  else if (GET_CODE (op) == MEM)
    return (GET_MODE_SIZE (mode) >= UNITS_PER_WORD || TARGET_DW_ENABLE);
  else
    return 0;
}

/* Return 1 if OP is an item in memory, given that we are in reload.  */

int
reload_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int regno = true_regnum (op);

  return (! CONSTANT_P (op)
	  && (regno == -1
	      || (GET_CODE (op) == REG
		  && REGNO (op) >= FIRST_PSEUDO_REGISTER)));
}

/* Given an object for which reload_memory_operand is true, return the address
   of the operand, taking into account anything that reload may do.  */

rtx
a29k_get_reloaded_address (op)
     rtx op;
{
  if (GET_CODE (op) == SUBREG)
    {
      if (SUBREG_WORD (op) != 0)
	abort ();

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) == REG)
    op = reg_equiv_mem[REGNO (op)];

  return find_replacement (&XEXP (op, 0));
}

/* Subfunction of the following function.  Update the flags of any MEM
   found in part of X.  */

static void
a29k_set_memflags_1 (x, in_struct_p, scalar_p, volatile_p, unchanging_p)
     rtx x;
     int in_struct_p, scalar_p, volatile_p, unchanging_p;
{
  int i;

  switch (GET_CODE (x))
    {
    case SEQUENCE:
    case PARALLEL:
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	a29k_set_memflags_1 (XVECEXP (x, 0, i), in_struct_p, scalar_p,
			     volatile_p, unchanging_p);
      break;

    case INSN:
      a29k_set_memflags_1 (PATTERN (x), in_struct_p, scalar_p, volatile_p,
			   unchanging_p);
      break;

    case SET:
      a29k_set_memflags_1 (SET_DEST (x), in_struct_p, scalar_p, volatile_p,
			   unchanging_p);
      a29k_set_memflags_1 (SET_SRC (x), in_struct_p, scalar_p, volatile_p,
			   unchanging_p);
      break;

    case MEM:
      MEM_IN_STRUCT_P (x) = in_struct_p;
      MEM_SCALAR_P (x) = scalar_p;
      MEM_VOLATILE_P (x) = volatile_p;
      RTX_UNCHANGING_P (x) = unchanging_p;
      break;

    default:
      break;
    }
}

/* Given INSN, which is either an INSN or a SEQUENCE generated to
   perform a memory operation, look for any MEMs in either a SET_DEST or
   a SET_SRC and copy the in-struct, unchanging, and volatile flags from
   REF into each of the MEMs found.  If REF is not a MEM, don't do
   anything.  */

void
a29k_set_memflags (insn, ref)
     rtx insn;
     rtx ref;
{
  /* Note that it is always safe to get these flags, though they won't
     be what we think if REF is not a MEM.  */
  int in_struct_p = MEM_IN_STRUCT_P (ref);
  int scalar_p = MEM_SCALAR_P (ref);
  int volatile_p = MEM_VOLATILE_P (ref);
  int unchanging_p = RTX_UNCHANGING_P (ref);

  if (GET_CODE (ref) != MEM
      || (! in_struct_p && ! volatile_p && ! unchanging_p))
    return;

  a29k_set_memflags_1 (insn, in_struct_p, scalar_p, volatile_p, unchanging_p);
}

/* Return 1 if OP is a comparison operator that we have in floating-point.  */

int
fp_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((mode == VOIDmode || mode == GET_MODE (op))
	  && (GET_CODE (op) == EQ || GET_CODE (op) == GT ||
	      GET_CODE (op) == GE));
}

/* Return 1 if OP is a valid branch comparison.  */

int
branch_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((mode == VOIDmode || mode == GET_MODE (op))
	  && (GET_CODE (op) == GE || GET_CODE (op) == LT));
}

/* Return 1 if OP is a load multiple operation.  It is known to be a
   PARALLEL and the first three sections will be tested.  */

int
load_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int count = XVECLEN (op, 0) - 2;
  int dest_regno;
  rtx src_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != MEM)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i + 2);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || REGNO (SET_DEST (elt)) != dest_regno + i
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
	  || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != i * 4)
	return 0;
    }

  return 1;
}

/* Similar, but tests for store multiple.  */

int
store_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int num_special = TARGET_NO_STOREM_BUG ? 2 : 1;
  int count = XVECLEN (op, 0) - num_special;
  int src_regno;
  rtx dest_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, 0)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i + num_special);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || REGNO (SET_SRC (elt)) != src_regno + i
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_addr)
	  || GET_CODE (XEXP (XEXP (SET_DEST (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1)) != i * 4)
	return 0;
    }

  return 1;
}

/* Given a special register REG and MASK, a value being masked against a
   quantity to which the special register is set, return 1 if the masking
   operation is built-in to the setting of that special register.  */

int
masks_bits_for_special (reg, mask)
     rtx reg;
     rtx mask;
{
   int needed_mask_value;

  if (GET_CODE (reg) != REG || GET_CODE (mask) != CONST_INT)
    abort ();

  switch (REGNO (reg))
    {
    case R_BP:
    case R_INT:
      needed_mask_value = 3;
      break;

    case R_FC:
      needed_mask_value = 31;
      break;

    case R_CR:
    case R_LRU:
      needed_mask_value = 255;
      break;

    case R_FPE:
      needed_mask_value = 511;
      break;

    case R_MMU:
      needed_mask_value = 0x3ff;
      break;

    case R_OPS:
    case R_CPS:
    case R_RBP:
    case R_FPS:
      needed_mask_value = 0xffff;
      break;

    case R_VAB:
      needed_mask_value = 0xffff0000;
      break;

    case R_Q:
    case R_CFG:
    case R_CHA:
    case R_CHD:
    case R_CHC:
    case R_TMC:
    case R_TMR:
    case R_PC0:
    case R_PC1:
    case R_PC2:
      return 0;

    default:
      abort ();
    }

   return (INTVAL (mask) & ~ needed_mask_value) == 0;
}

/* Return nonzero if this label is that of the return point, but there is
   a non-null epilogue.  */

int
epilogue_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return next_active_insn (op) == 0 && a29k_first_epilogue_insn != 0;
}

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

enum reg_class
secondary_reload_class (class, mode, in)
     enum reg_class class;
     enum machine_mode mode;
     rtx in;
{
  int regno = -1;
  enum rtx_code code = GET_CODE (in);

  if (! CONSTANT_P (in))
    {
      regno = true_regnum (in);

      /* A pseudo is the same as memory.  */
      if (regno == -1 || regno >= FIRST_PSEUDO_REGISTER)
	code = MEM;
    }

  /* If we are transferring between memory and a multi-word mode, we need
     CR.  */

  if (code == MEM && GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    return CR_REGS;

  /* If between memory and a mode smaller than a word without DW being
     enabled, we need BP.  */

  if (code == MEM && ! TARGET_DW_ENABLE
      && GET_MODE_SIZE (mode) < UNITS_PER_WORD)
    return BP_REGS;

  /* Otherwise, we can place anything into GENERAL_REGS and can put
     GENERAL_REGS into anything.  */
  if (class == GENERAL_REGS
      || (regno != -1
	  && (regno < R_BP
	      || (regno >= R_KR (0) && regno <= R_KR (31)))))
    return NO_REGS;

  /* We can place 16-bit constants into a special register.  */
  if (code == CONST_INT
      && (GET_MODE_BITSIZE (mode) <= 16 || (unsigned) INTVAL (in) <= 65535)
      && (class == BP_REGS || class == Q_REGS || class == SPECIAL_REGS))
    return NO_REGS;

  /* Otherwise, we need GENERAL_REGS.  */
  return GENERAL_REGS;
}

/* START is the zero-based incoming argument register index used (0 is 160,
   i.e., the first incoming argument register) and COUNT is the number used.

   Mark the corresponding incoming registers as neither fixed nor call used.
   For each register used for incoming arguments, we have one less local
   register that can be used.  So also mark some high-numbered registers as
   fixed.

   Return the first register number to use for the argument.  */

int
incoming_reg (start, count)
     int start;
     int count;
{
  int i;

  /* We only use 16 argument registers, so truncate at the end of the
     area.  */
  if (start + count > 16)
    count = 16 - start;

  if (! TARGET_NO_REUSE_ARGS)
    /* Mark all the used registers as not fixed and saved over calls.  */
    for (i = R_AR (start); i < R_AR (start + count); i++)
      {
	fixed_regs[i] = call_used_regs[i] = call_fixed_regs[i] = 0;
	CLEAR_HARD_REG_BIT (fixed_reg_set, i);
	CLEAR_HARD_REG_BIT (call_used_reg_set, i);
	CLEAR_HARD_REG_BIT (call_fixed_reg_set, i);
      }

  /* Shorten the maximum size of the frame.
     Remember that R_AR(-1,-2) are place holders for the caller's lr0,lr1.
     Make sure to keep the frame rounded to an even boundary.  Rounding up
     to an 8 byte boundary will use a slot.  Otherwise a frame with 121 local
     regs and 5 arguments will overrun the stack (121+1 + 5 + 2 > 128).  */
  /* ??? An alternative would be to never allocate one reg.  */
  for (i = (R_AR (0) - 2 - start - count) & ~1; i < R_AR (0) - 2 - start; i++)
    {
      fixed_regs[i] = call_used_regs[i] = call_fixed_regs[i] = 1;
      SET_HARD_REG_BIT (fixed_reg_set, i);
      SET_HARD_REG_BIT (call_used_reg_set, i);
      SET_HARD_REG_BIT (call_fixed_reg_set, i);
    }

  return R_AR (start);
}

/* Add CLOBBERs to CALL_INSN_FUNCTION_USAGE chain of INSN indicating
   that LR2 up to, but not including, OP are clobbered.  If OP is
   zero, indicate all parameter registers are clobbered.  */

void
a29k_clobbers_to (insn, op)
     rtx insn;
     rtx op;
{
  int i;
  int high_regno;

  if (op == 0)
    high_regno = R_LR (18);
  else if (GET_CODE (op) != REG || REGNO (op) < R_LR (0)
	   || REGNO (op) > R_LR (18))
    abort ();
  else
    high_regno = REGNO (op);

  for (i = R_LR (2); i < high_regno; i++)
    CALL_INSN_FUNCTION_USAGE (insn)
      = gen_rtx_EXPR_LIST (VOIDmode,
			   gen_rtx_CLOBBER (VOIDmode,
					    gen_rtx (REG, SImode, i)),
			   CALL_INSN_FUNCTION_USAGE (insn));
}

/* These routines are used in finding insns to fill delay slots in the
   epilogue.  */

/* Return 1 if the current function will adjust the register stack.  */

int
needs_regstack_p ()
{
  int i;
  rtx insn;

  if (frame_pointer_needed)
    return 1;

  /* If any local register is used, we need to adjust the regstack.  */
  for (i = R_LR (127); i >= R_LR (0); i --)
    if (regs_ever_live[i])
      return 1;

  /* We need a register stack if we make any calls.  */
  for (insn = get_insns (); insn; insn = next_insn (insn))
    if (GET_CODE (insn) == CALL_INSN
	|| (GET_CODE (insn) == INSN
	    && GET_CODE (PATTERN (insn)) == SEQUENCE
	    && GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == CALL_INSN))
      return 1;

  /* Otherwise, we don't.  */
  return 0;
}

/* Return 1 if X uses a local register.  */

int
uses_local_reg_p (x)
     rtx x;
{
  const char *fmt;
  int i, j;

  switch (GET_CODE (x))
    {
    case REG:
      return REGNO (x) >= R_LR (0) && REGNO (x) <= R_FP;

    case CONST_INT:
    case CONST:
    case PC:
    case CC0:
    case LABEL_REF:
    case SYMBOL_REF:
      return 0;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (uses_local_reg_p (XEXP (x, i)))
	    return 1;
	}
      else if (fmt[i] == 'E')
	{
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (uses_local_reg_p (XVECEXP (x, i, j)))
	      return 1;
	}
    }

  return 0;
}

/* Returns 1 if this function is known to have a null epilogue.  */

int
null_epilogue ()
{
  return (reload_completed && ! needs_regstack_p ()
	  && get_frame_size () == 0
	  && current_function_pretend_args_size == 0);
}

/* Write out the assembler form of an operand.  Recognize the following
   special options:

	%N means write the low-order 8 bits of the negative of the constant
	%Q means write a QImode operand (truncate constants to 8 bits)
	%M means write the low-order 16 bits of the constant
	%m means write the low-order 16 bits shifted left 16 bits
	%C means write the low-order 8 bits of the complement of the constant
	%b means write `f' is this is a reversed condition, `t' otherwise
	%B means write `t' is this is a reversed condition, `f' otherwise
	%J means write the 29k opcode part for a comparison operation
	%e means write the label with an extra `X' is this is the epilogue
	               otherwise the normal label name
	%E means write nothing if this insn has a delay slot,
		       a nop unless this is the epilogue label, in which case
		       write the first epilogue insn
	%F means write just the normal operand if the insn has a delay slot;
		       otherwise, this is a recursive call so output the
		       symbol + 4 and write the first prologue insn in the
		       delay slot.
	%L means write the register number plus one ("low order" register)
		       or the low-order part of a multi-word constant
	%O means write the register number plus two
	%P means write the register number plus three ("low order" of TImode)
	%S means write the number of words in the mode of the operand,
		       minus one (for CR)
        %V means write the number of elements in a PARALLEL minus 1
	%# means write nothing if we have a delay slot, "\n\tnop" otherwise
	%* means write the register name for TPC.  */

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     char code;
{
  char buf[100];

  /* These macros test for integers and extract the low-order bits.  */
#define INT_P(X)  \
((GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST_DOUBLE)	\
 && GET_MODE (X) == VOIDmode)

#define INT_LOWPART(X) \
  (GET_CODE (X) == CONST_INT ? INTVAL (X) : CONST_DOUBLE_LOW (X))

  switch (code)
    {
    case 'Q':
      if (GET_CODE (x) == REG)
	break;
      else if (! INT_P (x))
	output_operand_lossage ("invalid %%Q value");
      fprintf (file, "%d", INT_LOWPART (x) & 0xff);
      return;

    case 'C':
      if (! INT_P (x))
	output_operand_lossage ("invalid %%C value");
      fprintf (file, "%d", (~ INT_LOWPART (x)) & 0xff);
      return;

    case 'N':
      if (! INT_P (x))
	output_operand_lossage ("invalid %%N value");
      fprintf (file, "%d", (- INT_LOWPART (x)) & 0xff);
      return;

    case 'M':
      if (! INT_P (x))
	output_operand_lossage ("invalid %%M value");
      fprintf (file, "%d", INT_LOWPART (x) & 0xffff);
      return;

    case 'm':
      if (! INT_P (x))
	output_operand_lossage ("invalid %%m value");
      fprintf (file, "%d", (INT_LOWPART (x) & 0xffff) << 16);
      return;

    case 'b':
      if (GET_CODE (x) == GE)
	fprintf (file, "f");
      else
	fprintf (file, "t");
      return;

    case 'B':
      if (GET_CODE (x) == GE)
	fprintf (file, "t");
      else
	fprintf (file, "f");
      return;

    case 'J':
      /* It so happens that the RTX names for the conditions are the same as
	 the 29k's insns except for "ne", which requires "neq".  */
      fprintf (file, GET_RTX_NAME (GET_CODE (x)));
      if (GET_CODE (x) == NE)
	fprintf (file, "q");
      return;

    case 'e':
      if (optimize && flag_delayed_branch
	  && a29k_last_prologue_insn == 0 && epilogue_operand (x, VOIDmode)
	  && dbr_sequence_length () == 0)
	{
	  /* We need to output the label number of the last label in the
	     function, which is not necessarily X since there might be
	     a USE insn in between.  First go forward to the last insn, then
	     back up to a label.  */
	  while (NEXT_INSN (x) != 0)
	    x = NEXT_INSN (x);

	  while (GET_CODE (x) != CODE_LABEL)
	    x = PREV_INSN (x);

	  ASM_GENERATE_INTERNAL_LABEL (buf, "LX", CODE_LABEL_NUMBER (x));
	  assemble_name (file, buf);
	}
      else
	output_asm_label (x);
      return;

    case 'E':
      if (dbr_sequence_length ())
	;
      else if (a29k_last_prologue_insn)
	{
	  fprintf (file, "\n\t%s", a29k_last_prologue_insn);
	  a29k_last_prologue_insn = 0;
	}
      else if (optimize && flag_delayed_branch
	       && epilogue_operand (x, VOIDmode))
	{
	  fprintf (file, "\n\t%s", a29k_first_epilogue_insn);
	  a29k_first_epilogue_insn_used = 1;
	}
      else
	fprintf (file, "\n\tnop");
      return;
      
    case 'F':
      output_addr_const (file, x);
      if (dbr_sequence_length () == 0)
	{
	  /* If this doesn't have its delay slot filled, see if we need to
	     put the last insn of the prolog in it.  If not, see if this is
	     a recursive call.  If so, we can put the first insn of its
	     prolog in the delay slot.  Otherwise, write a nop.  */
	  if (a29k_last_prologue_insn)
	    {
	      fprintf (file, "\n\t%s", a29k_last_prologue_insn);
	      a29k_last_prologue_insn = 0;
	    }
	  else if (GET_CODE (x) == SYMBOL_REF
	      && ! strcmp (XSTR (x, 0), current_function_name))
	    fprintf (file, "+4\n\t%s,%d",
		     a29k_regstack_size >= 64 ? "const gr121" : "sub gr1,gr1",
		     a29k_regstack_size * 4);
	  else
	    fprintf (file, "\n\tnop");
	}
      return;

    case 'L':
      if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == DFmode)
	{
	  union real_extract u;

	  bcopy ((char *) &CONST_DOUBLE_LOW (x), (char *) &u, sizeof u);
	  fprintf (file, "$double1(%.20e)", u.d);
	}
      else if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x) + 1]);
      else
	output_operand_lossage ("invalid %%L value");
      return;

    case 'O':
      if (GET_CODE (x) != REG)
	output_operand_lossage ("invalid %%O value");
      fprintf (file, "%s", reg_names[REGNO (x) + 2]);
      return;

    case 'P':
      if (GET_CODE (x) != REG)
	output_operand_lossage ("invalid %%P value");
      fprintf (file, "%s", reg_names[REGNO (x) + 3]);
      return;

    case 'S':
      fprintf (file, "%d", (GET_MODE_SIZE (GET_MODE (x)) / UNITS_PER_WORD)-1);
      return;

    case 'V':
      if (GET_CODE (x) != PARALLEL)
	output_operand_lossage ("invalid %%V value");
      fprintf (file, "%d", XVECLEN (x, 0) - 2);
      return;

    case '#':
      if (dbr_sequence_length () == 0)
	{
	  if (a29k_last_prologue_insn)
	    {
	      fprintf (file, "\n\t%s", a29k_last_prologue_insn);
	      a29k_last_prologue_insn = 0;
	    }
	  else
	    fprintf (file, "\n\tnop");
	}
      return;

    case '*':
      fprintf (file, "%s", reg_names [R_TPC]);
      return;
    }

  if (GET_CODE (x) == REG)
    fprintf (file, "%s", reg_names [REGNO (x)]);

  else if (GET_CODE (x) == MEM)
    output_address (XEXP (x, 0));

  else if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == SUBREG
	   && GET_CODE (SUBREG_REG (XEXP (x, 0))) == CONST_DOUBLE)
    {
      union real_extract u;

      if (GET_MODE (SUBREG_REG (XEXP (x, 0))) == SFmode)
	fprintf (file, "$float");
      else
	fprintf (file, "$double%d", SUBREG_WORD (XEXP (x, 0)));
      bcopy ((char *) &CONST_DOUBLE_LOW (SUBREG_REG (XEXP (x, 0))),
	     (char *) &u, sizeof u);
      fprintf (file, "(%.20e)", u.d);
    }

  else if (GET_CODE (x) == CONST_DOUBLE
	   && GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    {
      union real_extract u;

      bcopy ((char *) &CONST_DOUBLE_LOW (x), (char *) &u, sizeof u);
      fprintf (file, "$%s(%.20e)",
	       GET_MODE (x) == SFmode ? "float" : "double0", u.d);
    }

  else
    output_addr_const (file, x);
}

/* This page contains routines to output function prolog and epilog code. */

/* Compute the size of the register stack, and determine if there are any
   call instructions.  */

static void
compute_regstack_size ()
{
  int i;
  rtx insn;

  /* See if we make any calls.  We need to set lr1 if so.  */
  a29k_makes_calls = 0;
  for (insn = get_insns (); insn; insn = next_insn (insn))
    if (GET_CODE (insn) == CALL_INSN
	|| (GET_CODE (insn) == INSN
	    && GET_CODE (PATTERN (insn)) == SEQUENCE
	    && GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == CALL_INSN))
      {
	a29k_makes_calls = 1;
	break;
      }

  /* Find the highest local register used.  */
  for (i = R_LR (127); i >= R_LR (0); i--)
    if (regs_ever_live[i])
      break;

  a29k_regstack_size = i - (R_LR (0) - 1);

  /* If calling routines, ensure we count lr0 & lr1.  */
  if (a29k_makes_calls && a29k_regstack_size < 2)
    a29k_regstack_size = 2;

  /* Count frame pointer and align to 8 byte boundary (even number of
     registers).  */
  a29k_regstack_size += frame_pointer_needed;
  if (a29k_regstack_size & 1) a29k_regstack_size++;
}

/*  Sets register names for incoming arguments and frame pointer.
    This can't be computed until after register allocation.  */

void
a29k_compute_reg_names ()
{
  int i;

  compute_regstack_size ();

  /* Set the names and numbers of the frame pointer and incoming argument
     registers.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    a29k_debug_reg_map[i] = i;

  reg_names[FRAME_POINTER_REGNUM] = reg_names[R_LR (a29k_regstack_size - 1)];
  a29k_debug_reg_map[FRAME_POINTER_REGNUM] = R_LR (a29k_regstack_size - 1);

  for (i = 0; i < 16; i++)
    {
      reg_names[R_AR (i)] = reg_names[R_LR (a29k_regstack_size + i + 2)];
      a29k_debug_reg_map[R_AR (i)] = R_LR (a29k_regstack_size + i + 2);
    }

  /* If using kernel register map, swap numbers for kernel and user
     registers.  */
  if (TARGET_KERNEL_REGISTERS)
    for (i = 0; i < 32; i++)
      {
	int tem = a29k_debug_reg_map[i];
	a29k_debug_reg_map[i] = a29k_debug_reg_map[R_KR (i)];
	a29k_debug_reg_map[R_KR (i)] = tem;
      }
}

/* Output function prolog code to file FILE.  Memory stack size is SIZE.  */

void
output_prolog (file, size)
     FILE *file;
     int size;
{
  int i;
  int arg_count = 0;
  rtx insn;
  unsigned int tag_word;

  /* See how many incoming arguments we have in registers.  */
  for (i = R_AR (0); i < R_AR (16); i++)
    if (! fixed_regs[i])
      arg_count++;

  /* The argument count includes the caller's lr0 and lr1.  */
  arg_count += 2;

  /* Compute memory stack size.  Add in number of bytes that the we should
     push and pretend the caller did and the size of outgoing arguments.
     Then round to a doubleword boundary.  */
  size += (current_function_pretend_args_size
	   + current_function_outgoing_args_size);
  size = (size + 7) & ~7;

  /* Write header words.  See if one or two word form.  */
  tag_word = (frame_pointer_needed ? 0x400000 : 0) + (arg_count << 16);

  if (size / 8 > 0xff)
    fprintf (file, "\t.word %d, 0x%0x\n", (size / 8) << 2,
	     0x800000 + tag_word);
  else
    fprintf (file, "\t.word 0x%0x\n", tag_word + ((size / 8) << 3));

  /* Define the function name.  */
  assemble_name (file, a29k_function_name);
  fprintf (file, ":\n");

  /* Push the register stack by the proper amount.  There are two possible
     ways to do this.  */
  if (a29k_regstack_size >= 256/4)
    fprintf (file, "\tconst %s,%d\n\tsub gr1,gr1,%s\n",
	     reg_names[R_TAV], a29k_regstack_size * 4, reg_names[R_TAV]);
  else if (a29k_regstack_size)
    fprintf (file, "\tsub gr1,gr1,%d\n", a29k_regstack_size * 4);

  /* Test that the registers are available.  */
  if (a29k_regstack_size)
    fprintf (file, "\tasgeu V_%sSPILL,gr1,%s\n",
	     TARGET_KERNEL_REGISTERS ? "K" : "", reg_names[R_RAB]);

  /* Set up frame pointer, if one is needed.  */
  if (frame_pointer_needed)
    fprintf (file, "\tsll %s,%s,0\n", reg_names[FRAME_POINTER_REGNUM],
	     reg_names[R_MSP]);

  /* Make room for any frame space.  There are three ways to do this.  */
  if (size >= 256)
    {
      fprintf (file, "\tconst %s,%d\n", reg_names[R_TAV], size);
      if (size >= 65536)
	fprintf (file, "\tconsth %s,%d\n", reg_names[R_TAV], size);
      if (TARGET_STACK_CHECK)
	fprintf (file, "\tcall %s,__msp_check\n", reg_names[R_TPC]);
      fprintf (file, "\tsub %s,%s,%s\n",
	       reg_names[R_MSP], reg_names[R_MSP], reg_names[R_TAV]);
    }
  else if (size)
    {
      if (TARGET_STACK_CHECK)
	fprintf (file, "\tcall %s,__msp_check\n", reg_names[R_TPC]);
      fprintf (file, "\tsub %s,%s,%d\n",
	       reg_names[R_MSP], reg_names[R_MSP], size);
    }

  /* If this routine will make calls, set lr1.  If we see an insn that
     can use a delay slot before a call or jump, save this insn for that
     slot (this condition is equivalent to seeing if we have an insn that
     needs delay slots before an insn that has a filled delay slot).  */
  a29k_last_prologue_insn = 0;
  if (a29k_makes_calls)
    {
      i = (a29k_regstack_size + arg_count) * 4;
      if (i >= 256)
	fprintf (file, "\tconst %s,%d\n\tadd lr1,gr1,%s\n",
		 reg_names[R_TAV], i, reg_names[R_TAV]);
      else
	{
	  if (optimize && flag_delayed_branch)
	    for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	      {
		if (GET_CODE (insn) == CODE_LABEL
		    || (GET_CODE (insn) == INSN
			&& GET_CODE (PATTERN (insn)) == SEQUENCE))
		  break;

		if (GET_CODE (insn) == NOTE
		    || (GET_CODE (insn) == INSN
			&& (GET_CODE (PATTERN (insn)) == USE
			    || GET_CODE (PATTERN (insn)) == CLOBBER)))
		  continue;

		if (num_delay_slots (insn) > 0)
		  {
		    a29k_last_prologue_insn = (char *) oballoc (100);
		    sprintf (a29k_last_prologue_insn, "add lr1,gr1,%d", i);
		    break;
		  }
	      }

	  if (a29k_last_prologue_insn == 0)
	    fprintf (file, "\tadd lr1,gr1,%d\n", i);
	}
    }

  /* Compute the first insn of the epilogue.  */
  a29k_first_epilogue_insn_used = 0;

  if (size == 0 && a29k_regstack_size == 0 && ! frame_pointer_needed)
    a29k_first_epilogue_insn = 0;
  else
    a29k_first_epilogue_insn = (char *) oballoc (100);

  if (frame_pointer_needed)
    sprintf (a29k_first_epilogue_insn, "sll %s,%s,0",
	     reg_names[R_MSP], reg_names[FRAME_POINTER_REGNUM]);
  else if (a29k_regstack_size)
    {
      if (a29k_regstack_size >= 256 / 4)
	sprintf (a29k_first_epilogue_insn, "const %s,%d",
		 reg_names[R_TAV], a29k_regstack_size * 4);
      else
	sprintf (a29k_first_epilogue_insn, "add gr1,gr1,%d",
		 a29k_regstack_size * 4);
    }
  else if (size)
    {
      if (size >= 256)
	sprintf (a29k_first_epilogue_insn, "const %s,%d",
		 reg_names[R_TAV], size);
      else
	sprintf (a29k_first_epilogue_insn, "add %s,%s,%d",
		 reg_names[R_MSP], reg_names[R_MSP], size);
    }
}

/* Call this after writing what might be the first instruction of the
   epilogue.  If that first insn was used in a delay slot, an intermediate
   label is written.  */

static void
check_epilogue_internal_label (file)
     FILE *file;
{
  rtx insn;

  if (! a29k_first_epilogue_insn_used)
    return;

  for (insn = get_last_insn ();
       GET_CODE (insn) != CODE_LABEL;
       insn = PREV_INSN (insn))
    ;

  ASM_OUTPUT_INTERNAL_LABEL (file, "LX", CODE_LABEL_NUMBER (insn));
  a29k_first_epilogue_insn_used = 0;
}

/* Output the epilog of the last procedure to file FILE.  SIZE is the memory
   stack size.  The register stack size is in the variable
   A29K_REGSTACK_SIZE.  */

void
output_epilog (file, size)
     FILE *file;
     int size;
{
  rtx insn;
  int locals_unavailable = 0;	/* True until after first insn
				   after gr1 update. */

  /* If we hit a BARRIER before a real insn or CODE_LABEL, we don't
     need to do anything because we are never jumped to.  */
  insn = get_last_insn ();
  if (GET_CODE (insn) == NOTE)
    insn = prev_nonnote_insn (insn);

  if (insn && GET_CODE (insn) == BARRIER)
    return;

  /* If a frame pointer was needed we must restore the memory stack pointer
     before adjusting the register stack.  */
  if (frame_pointer_needed)
    {
      fprintf (file, "\tsll %s,%s,0\n",
	       reg_names[R_MSP], reg_names[FRAME_POINTER_REGNUM]);
      check_epilogue_internal_label (file);
    }

  /* Restore the register stack.  There are two ways to do this.  */
  if (a29k_regstack_size)
    {
      if (a29k_regstack_size >= 256/4)
	{
	  fprintf (file, "\tconst %s,%d\n",
		   reg_names[R_TAV], a29k_regstack_size * 4);
	  check_epilogue_internal_label (file);
	  fprintf (file, "\tadd gr1,gr1,%s\n", reg_names[R_TAV]);
	}
      else
	{
	  fprintf (file, "\tadd gr1,gr1,%d\n", a29k_regstack_size * 4);
	  check_epilogue_internal_label (file);
	}
      locals_unavailable = 1;
    }

  /* Restore the memory stack pointer if there is no frame pointer.
     Adjust the size to include any pretend arguments and pushed
     arguments and round to doubleword boundary.  */
  size += (current_function_pretend_args_size
	   + current_function_outgoing_args_size);
  size = (size + 7) & ~7;

  if (size && ! frame_pointer_needed)
    {
      if (size >= 256)
	{
	  fprintf (file, "\tconst %s,%d\n", reg_names[R_TAV], size);
	  check_epilogue_internal_label (file);
	  locals_unavailable = 0;
	  if (size >= 65536)
	    fprintf (file, "\tconsth %s,%d\n", reg_names[R_TAV], size);
	  fprintf (file, "\tadd %s,%s,%s\n",
		   reg_names[R_MSP], reg_names[R_MSP], reg_names[R_TAV]);
	}
      else
	{
	  fprintf (file, "\tadd %s,%s,%d\n",
		   reg_names[R_MSP], reg_names[R_MSP], size);
	  check_epilogue_internal_label (file);
	  locals_unavailable = 0;
	}
    }

  if (locals_unavailable)
    {
      /* If we have an insn for this delay slot, write it.  */
      if (current_function_epilogue_delay_list)
	final_scan_insn (XEXP (current_function_epilogue_delay_list, 0),
			 file, 1, -2, 1);
      else
	fprintf (file, "\tnop\n");
    }

  fprintf (file, "\tjmpi lr0\n");
  if (a29k_regstack_size)
    fprintf (file, "\tasleu V_%sFILL,lr1,%s\n",
	     TARGET_KERNEL_REGISTERS ? "K" : "", reg_names[R_RFB]);
  else if (current_function_epilogue_delay_list)
    final_scan_insn (XEXP (current_function_epilogue_delay_list, 0),
		     file, 1, -2, 1);
  else
    fprintf (file, "\tnop\n");
}
